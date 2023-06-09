package flux.web

import flux.streams.operators.{QueueSubscriber, QueuedOperator}
import flux.streams.{Observable, Subject, Subscriber, Subscription}
import org.scalajs.dom.*

object Renderer {
  def render(parent: Node, elementChild: ElementChild): Unit = {
    val queue = Subject[() => Unit]()
    queue.subscribeNext(_())
    renderInternal(parent, elementChild)(queue)
  }

  case class Result(node: Node, subscriptions: Iterable[Subscription] = Nil)

  def renderInternal(parent: Node, elementChild: ElementChild, existing: Option[Node] = None)(implicit queue: QueueSubscriber): Result = {
    def replaceOrAppendChild[T <: Node](node: T, existing: Option[Node]) = {
      existing match {
        case Some(e) => parent.replaceChild(node, e)
        case None    => parent.appendChild(node)
      }
      node
    }

    elementChild match {
      case es: Iterable[NodeModel]                  =>
        (es.size until parent.childNodes.length)
          .map(parent.childNodes.item)
          .foreach(parent.removeChild)
        val existings = parent.childNodes.map(Some(_)) ++ (1 to (es.size - parent.childNodes.length)).map(_ => Option.empty[Node])
//        parent.childNodes.
        val results   = es
          .zip(existings)
          .map((e, existing) => renderInternal(parent, e, existing))
        Result(parent, results.flatMap(_.subscriptions))
      case ElementModel(name, properties, children) =>
        val updates                                = Subject[Element]()
        val element                                = existing match {
          case Some(e: Element) if e.tagName == name.toUpperCase => e
          case _                                                 => replaceOrAppendChild(document.createElement(name), existing)
        }
        // attributes
        def setAttribute(name: String, value: Any) = value match {
          case b: Boolean => if (b) element.setAttribute(name, "") else element.removeAttribute(name)
          case v          => element.setAttribute(name, value.toString)
        }
        val attributeProperties                    = properties.collect { case ap @ AttributeProperty(name, _) if name != "listKey" => ap }
        attributeProperties.foreach { case AttributeProperty(name, value) => setAttribute(name, value) }
        element.attributes.keySet
          .diff(attributeProperties.map(_.name).toSet)
          .foreach(element.removeAttribute)
        // ref
        properties.collect { case r: RefProperty[Element] => r }.foreach(_.subscriber.onNext(element))

        val propertiesSubscriptions = properties.collect {
          case ObservableAttributeProperty(name, observable)  =>
            observable
              .subscribeNext(value => {
                setAttribute(name, value)
                updates.onNext(element)
              })
          case EventProperty(event, subscriber)               => Observable.fromEventListener(element, event).subscribe(subscriber)
          case OnComponentUpdateProperty[Element](subscriber) => updates.subscribe(subscriber)
        }

        val childNodes                            = element.childNodes
        val subscriptions: Iterable[Subscription] =
          children
            .zip(childNodes.map(Some(_)) ++ (1 to (children.size - element.childNodes.length)).map(_ => None))
            .map((e, n) => renderInternal(element, e, n))
            .flatMap(_.subscriptions)
            ++ propertiesSubscriptions

        childNodes.drop(children.size).foreach(element.removeChild)
        updates.onNext(element)
        Result(element, subscriptions)
      case s: String                                =>
        Result(existing match {
          case Some(t: Text) =>
            t.data = s
            t
          case _             => replaceOrAppendChild(document.createTextNode(s), existing)
        })
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
      case _                                        => Result(replaceOrAppendChild(document.createComment("rest"), existing))
    }
  }

}
