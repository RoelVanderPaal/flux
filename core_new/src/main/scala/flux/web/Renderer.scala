package flux.web

import flux.streams.operators.{QueueSubscriber, QueuedOperator}
import flux.streams.{Observable, Subject, Subscriber, Subscription}
import org.scalajs.dom.{document, Element, Node, NonDocumentTypeChildNode}

object Renderer {
  def render(parent: Node, elementChild: ElementChild): Unit = {
    val queue = Subject[() => Unit]()
    queue.subscribeNext(_())
    renderInternal(parent, elementChild)(queue)
  }

  case class Result(node: Node, subscriptions: Iterable[Subscription] = Nil)

  def renderInternal(parent: Node, elementChild: ElementChild, existing: Option[Node] = None)(implicit queue: QueueSubscriber): Result = {
    def replaceOrAppendChild(parent: Node, node: Node, existing: Option[Node]) = existing match {
      case Some(e) => {
//        println(elementChild)
//        println(parent)
//        println(e)
//        println(node)
//        println()
        parent.replaceChild(node, e)
      }
      case None    => parent.appendChild(node)
    }

    elementChild match {
      case ElementModel(name, properties, children) =>
        val updates                       = Subject[Element]()
        val element                       = document.createElement(name)
        replaceOrAppendChild(parent, element, existing)
        val propertiesSubscriptions       = properties.map {
          case AttributeProperty(name, v)           =>
            element.setAttribute(name, v.toString)
            None
          case ObservableAttributeProperty(name, o) =>
            Some(
              o.map(_.toString)
                .subscribeNext(str => {
                  element.setAttribute(name, str)
                  updates.onNext(element)
                })
            )
          case EventProperty(event, subscriber)     =>
            val o = Observable.fromEventListener(element, event)
            Some(o.subscribe(subscriber))
//            Some(QueuedOperator(o, queue).subscribe(subscriber))
          case _                                    => None
        }
        properties.collect { case RefProperty[Element](subscriber) => subscriber.onNext(element) }
        val onComponentUpdateSubscription = properties.collect { case OnComponentUpdateProperty[Element](subscriber) =>
          updates.subscribe(subscriber)
        }

        val subscriptions: Iterable[Subscription] =
          children
            .map(renderInternal(element, _, None))
            .flatMap(_.subscriptions)
            ++ propertiesSubscriptions.flatten
            ++ onComponentUpdateSubscription
        updates.onNext(element)
        Result(element, subscriptions)
      case s: String                                =>
        val text = document.createTextNode(s)
        replaceOrAppendChild(parent, text, existing)
        Result(text)
      case o: Observable[NodeModel]                 =>
        val node: Node   = document.createComment("placeholder")
        parent.appendChild(node)
        val subscription = o
          .fold(Result(node))((existing, e) => {
            existing.subscriptions.foreach(_.unsubscribe())
            renderInternal(parent, e, Some(existing.node))
          })
          .subscribe(new Subscriber[Result] {
            override def onNext(t: Result): Unit = {}

            override def onCompleted: Unit = {}
          })
        Result(node, List(subscription))
      case _                                        =>
        val comment = document.createComment("rest")
        replaceOrAppendChild(parent, comment, existing)
        Result(comment)
    }
  }

}
