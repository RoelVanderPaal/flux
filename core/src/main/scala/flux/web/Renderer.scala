package flux.web
import flux.streams.constructor.{AbstractObservable, BasicSubscription, EventListenerObservable}
import flux.streams.operators.{QueueSubscriber, QueuedOperator}
import flux.streams.{Observable, Subject, Subscriber, Subscription}
import org.scalajs.dom.*

import scala.annotation.nowarn

object Renderer {
  var classNames = Set.empty[String]
  case class ReturnState(nodeHolder: NodeHolder, subscriptionsHolder: SubscriptionsHolder) {
    def node          = nodeHolder.node
    def subscriptions = subscriptionsHolder.subscriptions
  }
  case class NodeHolder()                                                                  {
    var node: Node = _
  }
  object NodeHolder                                                                        {
    def apply(node: Node): NodeHolder = {
      val holder = NodeHolder()
      holder.node = node
      holder
    }
  }
  case class SubscriptionsHolder()                                                         {
    var subscriptions: Iterable[Subscription] = _
  }
  object SubscriptionsHolder                                                               {
    def empty: SubscriptionsHolder                                        = create(Iterable.empty[Subscription])
    def apply(subscription: Subscription): SubscriptionsHolder            = create(Iterable(subscription))
    def apply(subscriptions: Iterable[Subscription]): SubscriptionsHolder = create(subscriptions)

    private def create(subscriptions: Iterable[Subscription]): SubscriptionsHolder = {
      val holder = SubscriptionsHolder()
      holder.subscriptions = subscriptions
      holder
    }
  }

  case class PreviousState(node: Node, elementChild: ElementChild)
  def render(parent: Node, elementChild: ElementChild): Unit = {
    val queue = Subject[() => Unit]()
    queue.subscribeNext(_())
    renderInternal(parent, elementChild, None)(queue)
  }

  private def renderInternal(
    parent: Node,
    elementChild: ElementChild,
    previousState: Option[PreviousState] = None
  )(implicit queue: QueueSubscriber
  ): ReturnState = {
    def replaceOrAppendChild[T <: Node](node: T, existing: Option[Node], parent: Node): T = {
      existing match {
        case Some(e) => parent.replaceChild(node, e)
        case None    => parent.appendChild(node)
      }
      node
    }

    elementChild match {
      case EmptyNode                                =>
        val node = previousState match {
          case Some(PreviousState(currentComment: Comment, EmptyNode)) => currentComment
          case _                                                       =>
            val commentNode = document.createComment("emptyNode")
            replaceOrAppendChild(commentNode, previousState.map(_.node), parent)
            commentNode
        }
        ReturnState(NodeHolder(node), SubscriptionsHolder.empty)
      case ElementModel(name, properties, children) =>
        val (element, previousProperties, previousChildren) = previousState match {
          case Some(PreviousState(node: Element, ElementModel(previousName, previousProperties, previousChildren)))
              if previousName == name =>
            (node, previousProperties, previousChildren)
          case _ =>
            (
              replaceOrAppendChild(document.createElement(name), previousState.map(_.node), parent),
              Iterable.empty[Property[_, _]],
              Iterable.empty[ElementChild]
            )
        }
        // children
        val previousChildStates                             = element.childNodes
          .zip(previousChildren)
          .map { case (n: Node, e: ElementChild) => PreviousState(n, e) }
          .map(Option.apply)
        val childSubscriptions                              = children
          .zip(
            previousChildStates
              .take(children.size)
              .toSeq
              .padTo(children.size, None)
          )
          .map { case (e, p) => renderInternal(element, e, p) }
          .flatMap(_.subscriptions)
        previousChildStates.drop(children.size).flatten.map(_.node).foreach(element.removeChild)

        // properties
        val propertySubscriptions = properties.toSet.diff(previousProperties.toSet).flatMap(handleProperty(element))
        // remove previous properties
        previousProperties.map(_.key).toSet.diff(properties.map(_.key).toSet).foreach(element.removeAttribute)

        ReturnState(NodeHolder(element), SubscriptionsHolder(childSubscriptions ++ propertySubscriptions))
      case o: Observable[ElementChild]              =>
        val nodeHolder                = previousState.map(s => NodeHolder(s.node)).getOrElse {
          val comment = document.createComment("placeholder for stream")
          replaceOrAppendChild(comment, None, parent)
          NodeHolder(comment)
        }
        val parentSubscriptionsHolder = SubscriptionsHolder.empty
        val subscription              = o.subscribe(new Subscriber[ElementChild] {
          var latest                                 = Option.empty[ElementChild]
          val r                                      = ReturnState(
            nodeHolder,
            SubscriptionsHolder.empty
          )
          override def onNext(t: ElementChild): Unit = {
            r.subscriptions.foreach(_.unsubscribe())
            parentSubscriptionsHolder.subscriptions = parentSubscriptionsHolder.subscriptions.filterNot(r.subscriptions.toList.contains)
            val renderResult = renderInternal(parent, t, Some(PreviousState(nodeHolder.node, latest.getOrElse("placeholder"))))
            latest = Some(t)
            r.nodeHolder.node = renderResult.nodeHolder.node
            r.subscriptionsHolder.subscriptions = renderResult.subscriptionsHolder.subscriptions
            parentSubscriptionsHolder.subscriptions ++= renderResult.subscriptionsHolder.subscriptions
          }

          override def onCompleted: Unit = {}
        })
        parentSubscriptionsHolder.subscriptions = parentSubscriptionsHolder.subscriptions ++ Iterable(subscription)
        ReturnState(nodeHolder, parentSubscriptionsHolder)
      case text: String                             =>
        val node = previousState match {
          case Some(PreviousState(currentText: Text, _: String)) =>
            currentText.data = text
            currentText
          case _                                                 =>
            val textNode = document.createTextNode(text)
            replaceOrAppendChild(textNode, previousState.map(_.node), parent)
            textNode
        }
        ReturnState(NodeHolder(node), SubscriptionsHolder.empty)
    }
  }

  val ATTRIBUTE_MAPPINGS = Map("className" -> "class")
  private def handleProperty[Value](element: Element)(p: Property[Value, _])(implicit queue: QueueSubscriber): Option[Subscription] = {
    def setAttribute[Value](key: String, value: Value) = value match {
      case ps: Iterable[CssProperty | SelectorProperty] =>
        val className = s"flux-${ps.map(_.toString).hashCode()}"
        if (!classNames.contains(className)) {
          classNames += className
          addStyleSheet(cssProperties2String(className, ps))
        }
        element.setAttribute("class", className)
      case _                                            =>
        val k = ATTRIBUTE_MAPPINGS.getOrElse(key, key)
        (k, value) match {
          case _ =>
            value match {
              case b: Boolean => if (b) element.setAttribute(k, "") else element.removeAttribute(k)
              case v          => element.setAttribute(k, v.toString)
            }
        }

    }

    p match {
      case SubscriberProperty(key, subscriber: Subscriber[Value]) =>
        val value1 = Observable.fromEventListener(element, key)
        val value2 = QueuedOperator(value1, queue)
        Some(value2.subscribe(subscriber))
      case AttributeProperty(key, value: Value) if key == "id"    =>
        element.id = value.toString
        None
      case AttributeProperty(key, value: Value)                   =>
        setAttribute(key, value)
        None
      case ObservableProperty(key, o: Observable[Value])          =>
        Some(o.subscribe(new Subscriber[Value] {
          override def onNext(t: Value): Unit = setAttribute(key, t)

          override def onCompleted: Unit = {}
        }))
    }
  }

  private def cssProperties2String(className: String, properties: Iterable[CssProperty | SelectorProperty]): String = {
    val body         = properties
      .collect { case p: CssProperty => p }
      .map(p => s"${p.key}:${p.value}")
      .toList
      .sorted
      .mkString(";")
    val selectorBody = properties
      .collect { case p: SelectorProperty => p }
      .toList
      .sortBy(_.selector.toString)
      .map(p => cssProperties2String(className + ":" + p.selector.toString.substring(1), p.value))
      .map(v => "\n" + v)
      .mkString("")
    s".$className{$body}$selectorBody"
  }

  private def addStyleSheet(css: String) = {
    val styleSheet = document.createElement("style")
    styleSheet.textContent = css
    document.head.appendChild(styleSheet)
  }

}
