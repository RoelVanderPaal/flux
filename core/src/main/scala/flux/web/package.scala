package flux
import flux.streams.Observable

package object web {
  type NodeModel    = String | ElementModel
  type ElementChild = NodeModel | Observable[NodeModel]
  case class ElementModel(name: String, children: Seq[ElementChild])

  trait ElementModelFactory(name: String) {
    def apply(children: ElementChild*): ElementModel = ElementModel(name, children)
  }

  case object div extends ElementModelFactory("div")
  case object h1  extends ElementModelFactory("h1")
}
