package flux.web

import org.scalajs.dom.{document, Comment}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class RendererTest extends AnyFunSuite with Matchers {
  test("render empty") {
    val parent = document.createElement("div")
    Renderer.renderInternal(parent, EmptyNode, None)
//    parent.childNodes.toList.foreach(println)
  }
}
