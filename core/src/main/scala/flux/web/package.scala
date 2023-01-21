package flux
import flux.streams.{Observable, Subscriber}
import org.scalajs.dom.{Event, MouseEvent}

package object web {
  type NodeModel    = Any | ElementModel
  type ElementChild = NodeModel | Observable[NodeModel]

  trait Scope
  trait ElementScope           extends Scope
  trait HTMLElementScope       extends Scope
  trait HTMLButtonElementScope extends Scope

  sealed trait Property[Value, +Scope] {
    def key: Name[Value, Scope]
  }
  case class SimpleProperty[Value, Scope](key: WritableName[Value, Scope], value: Value)                 extends Property[Value, Scope]
  case class ObservableProperty[Value, Scope](key: WritableName[Value, Scope], value: Observable[Value]) extends Property[Value, Scope]
  case class SubscriberProperty[Value <: Event, Scope](key: EventName[Value, Scope], value: Subscriber[Value])
      extends Property[Value, Scope]

  sealed trait Name[Value, +Scope]

  trait WritableName[Value, Scope] extends Name[Value, Scope] {
    def :=(value: Value)             = SimpleProperty[Value, Scope](this, value)
    def :=(value: Observable[Value]) = ObservableProperty[Value, Scope](this, value)

  }

  trait EventName[Value <: Event, Scope] extends Name[Value, Scope] {
    def :=(value: Subscriber[Value]) = SubscriberProperty(this, value)
  }

  case class ElementModel(name: String, properties: Seq[Property[_, _]], children: Seq[ElementChild])

  class ElementModelFactoryWithoutChildren(name: String, properties: Seq[Property[_, _]]) {
    def apply(children: ElementChild*): ElementModel = ElementModel(name, properties, children)
  }

  trait ElementModelFactory[Scope](name: String) {
//    def apply(children: ElementChild*): ElementModel = ElementModel(name, List.empty[Property[_, Scope]], children)

    def apply(properties: Property[_, Scope]*) = new ElementModelFactoryWithoutChildren(name, properties)
  }

  case class CssProperty(key: CssName, value: String)
  trait CssName(val name: String) {
    def :=(value: String) = CssProperty(this, value)
  }

  case object div    extends ElementModelFactory[ElementScope | HTMLElementScope]("div")
  case object h1     extends ElementModelFactory[ElementScope | HTMLElementScope]("h1")
  case object button extends ElementModelFactory[ElementScope | HTMLElementScope | HTMLButtonElementScope]("button")

  case object disabled   extends WritableName[Boolean, HTMLButtonElementScope]
  case object classStyle extends WritableName[Iterable[CssProperty], HTMLElementScope]

  case object onclick extends EventName[MouseEvent, HTMLElementScope]

  case object backgroundColor extends CssName("background-color")

}
