package flux

import flux.streams.Subscriber
import org.scalajs.dom.{document, Node}

object Renderer {
  def render[V: Viewable](parent: Node, viewable: V): Unit = {
    renderIterableView(parent, viewable.toViews)
  }

  private def renderIterableView(parent: Node, vs: Iterable[View], existings: Iterable[Node] = Nil): Unit = {
    vs.foreach(renderView(parent, _))
  }

  private def renderView(parent: Node, v: View): Unit = {
    v match {
      case ElementModel(name, properties, children) =>
        val element = document.createElement(name)
        parent.appendChild(element)

        // attributes
        def setAttribute(name: String, value: Any) = value match {
          case b: Boolean => if (b) element.setAttribute(name, "") else element.removeAttribute(name)
          case v          => element.setAttribute(name, value.toString)
        }
        val attributeProperties                    = properties.collect { case ap: AttributeProperty[_, _] => ap }
        attributeProperties.foreach { case AttributeProperty(name, value) => setAttribute(name, value) }
        element.attributes.keySet
          .filterNot(_.startsWith("data-"))
          .diff(attributeProperties.map(_.name).toSet)
          .foreach(element.removeAttribute)

        children.foreach(renderView(element, _))
      case StringView(s)                            => parent.appendChild(document.createTextNode(s))
      case ObservableView(o)                        =>
//        val node = document.createComment("placeholder")
//        parent.appendChild(node)
        o.subscribe(new Subscriber[Iterable[View]] {
          override def onNext(views: Iterable[View]): Unit = renderIterableView(parent, views)

          override def onCompleted: Unit = {}
        })
      case EmptyNode                                => parent.appendChild(document.createComment("empty"))
    }
  }

}
