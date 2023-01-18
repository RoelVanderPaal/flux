import flux.web.*
import org.scalajs.dom.{document, MouseEvent}

object Main extends App {
  Renderer.render(document.body, div(h1("Header"), "hello", div("one", "two", "three")))
}
