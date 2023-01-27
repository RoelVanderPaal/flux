package flux.web
import flux.streams.constructor.AbstractObservable
import flux.streams.{Observable, Subscriber, Subscription}
import org.scalajs.dom.*

import scala.annotation.nowarn

object Renderer {
  var classNames = Set.empty[String]
  case class Result(nodeHolder: NodeHolder, subscriptionsHolder: SubscriptionsHolder) {
    def node          = nodeHolder.node
    def subscriptions = subscriptionsHolder.subscriptions
  }
  case class NodeHolder()                                                             {
    var node: Node = _
  }
  object NodeHolder                                                                   {
    def apply(node: Node): NodeHolder = {
      val holder = NodeHolder()
      holder.node = node
      holder
    }
  }
  case class SubscriptionsHolder()                                                    {
    var subscriptions: Iterable[Subscription] = _
  }
  object SubscriptionsHolder                                                          {
    def empty: SubscriptionsHolder                                        = create(Iterable.empty[Subscription])
    def apply(subscription: Subscription): SubscriptionsHolder            = create(Iterable(subscription))
    def apply(subscriptions: Iterable[Subscription]): SubscriptionsHolder = create(subscriptions)

    private def create(subscriptions: Iterable[Subscription]): SubscriptionsHolder = {
      val holder = SubscriptionsHolder()
      holder.subscriptions = subscriptions
      holder
    }
  }

  def render(
    parent: Node,
    nodeModel: ElementChild,
    previousNodeModel: Option[ElementChild] = None,
    result: Option[Result] = None
  ): Result = {
    nodeModel match {
      case ElementModel(name, properties, children) =>
        val element               = document.createElement(name)
        replaceOrAppendChild(element, result.map(_.node), parent)
        val propertySubscriptions = properties.flatMap(handleProperty(element))
        val childSubscriptions    = children.map((nodeModel: ElementChild) => render(element, nodeModel)).flatMap(_.subscriptions)
        Result(NodeHolder(element), SubscriptionsHolder(childSubscriptions ++ propertySubscriptions))
      case o: Observable[NodeModel]                 =>
        val nodeHolder                = result.map(_.nodeHolder).getOrElse {
          val comment = document.createComment("placeholder for stream")
          replaceOrAppendChild(comment, None, parent)
          NodeHolder(comment)
        }
        val parentSubscriptionsHolder = SubscriptionsHolder.empty
        val subscription              = o.subscribe(new Subscriber[NodeModel] {
          var latest                              = Option.empty[NodeModel]
          val r                                   = Result(
            nodeHolder,
            SubscriptionsHolder.empty
          )
          override def onNext(t: NodeModel): Unit = {
            r.subscriptions.foreach(_.unsubscribe())
            parentSubscriptionsHolder.subscriptions = parentSubscriptionsHolder.subscriptions.filterNot(r.subscriptions.toList.contains)
            val renderResult = render(parent, t, latest, Some(r))
            latest = Some(t)
            r.nodeHolder.node = renderResult.nodeHolder.node
            r.subscriptionsHolder.subscriptions = renderResult.subscriptionsHolder.subscriptions
            parentSubscriptionsHolder.subscriptions ++= renderResult.subscriptionsHolder.subscriptions
          }

          override def onCompleted: Unit = {}
        })
        parentSubscriptionsHolder.subscriptions = parentSubscriptionsHolder.subscriptions ++ Iterable(subscription)
        Result(nodeHolder, parentSubscriptionsHolder)
      case text: Any                                =>
        val n = result.map(_.node) match {
          case Some(currentText: Text) =>
            currentText.data = text.toString
            currentText
          case nonText                 =>
            val text1 = document.createTextNode(text.toString)
            replaceOrAppendChild(text1, nonText, parent)
            text1
        }
        Result(NodeHolder(n), SubscriptionsHolder.empty)
    }
  }

  private def handleProperty[Value](element: Element)(p: Property[Value, _]): Option[Subscription] = {
    def setAttribute[Value](key: Name[_, _], value: Value) = value match {
      case ps: Iterable[CssProperty | SelectorProperty] =>
        val className = s"flux-${ps.map(_.toString).hashCode()}"
        if (!classNames.contains(className)) {
          classNames += className
          addStyleSheet(cssProperties2String(className, ps))
        }
        element.setAttribute("class", className)
      case _                                            =>
        value match {
          case b: Boolean => if (b) Some("") else None
          case v          => Some(v.toString)
        } match {
          case Some(s) => element.setAttribute(key.name, s)
          case None    => element.removeAttribute(key.name)
        }

    }

    p match {
      case SubscriberProperty(key, subscriber: Subscriber[Value]) =>
        Observable.fromEventListener(element, key.name).subscribe(subscriber)
        None
      case SimpleProperty(key, value: Value)                      =>
        setAttribute(key, value)
        None
      case ObservableProperty(key, o: Observable[Value])          =>
        Some(o.subscribe(new Subscriber[Value] {
          override def onNext(t: Value): Unit = setAttribute(key, t)

          override def onCompleted: Unit = {}
        }))
    }
  }

  private def replaceOrAppendChild(node: Node, existing: Option[Node], parent: Node): Unit = {
    existing match {
      case Some(e) => parent.replaceChild(node, e)
      case None    => parent.appendChild(node)
    }
  }

  private def cssProperties2String(className: String, properties: Iterable[CssProperty | SelectorProperty]): String = {
    val body         = properties
      .collect { case p: CssProperty => p }
      .map(p => s"${p.key.name}:${p.value}")
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
