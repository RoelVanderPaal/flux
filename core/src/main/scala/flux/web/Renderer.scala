package flux.web
import flux.streams.Observable
import org.scalajs.dom.*

import scala.annotation.nowarn

object Renderer {
  def render(parent: Element, nodeModel: NodeModel): Unit = {
    val node = nodeModel match {
      case text: ByteString             => document.createTextNode(text)
      case ElementModel(name, children) =>
        val element = document.createElement(name)
        children.foreach {
          case n: NodeModel             => render(element, n)
          case o: Observable[NodeModel] =>
        }
        element
    }
    parent.appendChild(node)
  }

}
