package flux.web

import flux.web.Renderer.PreviousState
import org.scalajs.dom.{document, Comment, Node}

trait NodeLike

case class PreviousState(node: Node, elementChild: ElementChild)
object RendererNew {
  def render(parent: NodeLike, elementChild: ElementChild, previousState: Option[PreviousState] = None) = {
    elementChild match {
      case EmptyNode =>
        val node = previousState match {
          case Some(PreviousState(currentComment: Comment, EmptyNode)) => currentComment
          case _                                                       =>
            val commentNode = document.createComment("emptyNode")
//            replaceOrAppendChild(commentNode, previousState.map(_.node), parent)
            commentNode
        }
        node
    }
  }

//  private def replaceOrAppendChild[T <: Node](node: T, existing: Option[Node], parent: NodeLike): T = {
//    existing match {
//      case Some(e) => parent.replaceChild(node, e)
//      case None    => parent.appendChild(node)
//    }
//    node
//  }

}
