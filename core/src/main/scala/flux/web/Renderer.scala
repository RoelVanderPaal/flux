package flux.web
import flux.streams.constructor.AbstractObservable
import flux.streams.{Observable, Subscriber}
import org.scalajs.dom.*

import scala.annotation.nowarn

object Renderer {
  var classNames = Set.empty[String]

  def render(parent: Element, nodeModel: ElementChild, currentNode: Option[Node] = None): Option[Node] = {
    def replaceOrAppendChild(node: Node, existing: Option[Node], parent: Element = parent): Option[Node] = {
      existing match {
        case Some(e) => parent.replaceChild(node, e)
        case None    => parent.appendChild(node)
      }
      Some(node)
    }
    def handleProperty[Value](element: Element)(p: Property[Value, _])                                   = {
      def setAttribute[Value](key: WritableName[_, _], value: Value) = value match {
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
            case Some(s) => element.setAttribute(key.toString, s)
            case None    => element.removeAttribute(key.toString)
          }

      }
      p match {
        case SubscriberProperty(key, subscriber: Subscriber[Value]) =>
          val o = new AbstractObservable[Value] {
            val listener = (e: Value) => subscribers.foreach(_.onNext(e))
            val `type`   = key.toString.stripPrefix("on")

            override def onStart(): Unit = element.addEventListener[Value](`type`, listener)

            override def onStop(): Unit = element.removeEventListener[Value](`type`, listener)
          }
          o.subscribe(subscriber)
        case SimpleProperty(key, value: Value)                      => setAttribute(key, value)
        case ObservableProperty(key, o: Observable[Value])          =>
          o.subscribe(new Subscriber[Value] {
            override def onNext(t: Value): Unit = setAttribute(key, t)
            override def onCompleted: Unit      = {}
          })
      }
    }

    nodeModel match {
      case ElementModel(name, properties, children) =>
        val element = document.createElement(name)
        parent.appendChild(element)
        properties.foreach(handleProperty(element))
        children.foreach {
          case n: NodeModel             => render(element, n)
          case o: Observable[NodeModel] =>
            o.subscribe(new Subscriber[NodeModel] {
              var currentNode = replaceOrAppendChild(document.createComment("placeholder for stream"), None, element)

              override def onNext(t: NodeModel): Unit = {
                currentNode = render(element, t, currentNode)
              }

              override def onCompleted: Unit = {}
            })
        }
        Some(element)
      case o: Observable[NodeModel]                 =>
        var currentNode = replaceOrAppendChild(document.createComment("placeholder for stream"), None)
        o.subscribe(new Subscriber[NodeModel] {

          override def onNext(t: NodeModel): Unit = {
            currentNode = render(parent, t, currentNode)
          }

          override def onCompleted: Unit = {}
        })
        None
      case text: Any                                =>
        currentNode match {
          case Some(currentText: Text) =>
            currentText.data = text.toString
            currentNode
          case existing                => replaceOrAppendChild(document.createTextNode(text.toString), existing)
        }

    }

  }

  def cssProperties2String(className: String, properties: Iterable[CssProperty | SelectorProperty]): String = {
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

  def addStyleSheet(css: String) = {
    val styleSheet = document.createElement("style")
    styleSheet.textContent = css
    document.head.appendChild(styleSheet)
  }

}
