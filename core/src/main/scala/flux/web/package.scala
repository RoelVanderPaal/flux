package flux
import flux.streams.{Observable, Subscriber}
import org.scalajs.dom.{Event, MouseEvent}

package object web {

  type NodeModel    = Any | ElementModel
  type ElementChild = NodeModel | Observable[NodeModel]

  private trait Scope
  private trait ElementScope           extends Scope
  private trait HTMLElementScope       extends Scope
  private trait HTMLButtonElementScope extends Scope

  sealed private trait Property[Value, +Scope] {
    def key: Name[Value, Scope]
  }
  object Property                              {
    def unsafe[Value](key: String, v: Value)                       = SimpleProperty(createName(key), v)
    def unsafeObservable[Value](key: String, v: Observable[Value]) = ObservableProperty(createName(key), v)
    def unsafeSubscriber[Value](key: String, v: Subscriber[Value]) = SubscriberProperty(createName(key), v)

    private def createName[Value](key: String) = {
      new Name[Value, ElementScope] {
        override def name: String = key
      }
    }
  }
  private case class SimpleProperty[Value, Scope](key: Name[Value, Scope], value: Value) extends Property[Value, Scope]
  private case class ObservableProperty[Value, Scope](key: Name[Value, Scope], value: Observable[Value]) extends Property[Value, Scope]
  private case class SubscriberProperty[Value <: Event, Scope](key: Name[Value, Scope], value: Subscriber[Value])
      extends Property[Value, Scope]

  sealed private trait Name[Value, +Scope] {
    def name: String
  }

  private trait WritableName[Value, Scope] extends Name[Value, Scope] {
    def :=(value: Value)             = SimpleProperty[Value, Scope](this, value)
    def :=(value: Observable[Value]) = ObservableProperty[Value, Scope](this, value)

    override def name: String = this.toString
  }

  private trait EventName[Value <: Event, Scope] extends Name[Value, Scope] {
    def :=(value: Subscriber[Value]) = SubscriberProperty(this, value)

    override def name: String = this.toString.stripPrefix("on")
  }

  private case class ElementModel(name: String, properties: Iterable[Property[_, _]], children: Iterable[ElementChild])
  object ElementModel {
    def unsafe(name: String, properties: Seq[Property[_, _]], children: Iterable[ElementChild]) = ElementModel(name, properties, children)
  }

  private class ElementModelFactoryWithoutChildren(name: String, properties: Iterable[Property[_, _]]) {
    def apply(children: ElementChild*): ElementModel = ElementModel(name, properties, children)
  }

  trait ElementModelFactory[Scope](name: String) {
    def apply(children: ElementChild*): ElementModel = ElementModel(name, List.empty[Property[_, Scope]], children)

    def apply(properties: Property[_, Scope]*) = new ElementModelFactoryWithoutChildren(name, properties)
  }

  private case class CssProperty(key: CssName, value: String)
  object CssProperty                      {
    def unsafe(key: String, value: String) = CssProperty(new CssName(key) {}, value)
  }
  private trait CssName(val name: String) {
    def :=(value: String) = CssProperty(this, value)
  }
  private case class SelectorProperty(selector: PseudoClass, value: Iterable[CssProperty])

  trait PseudoClass {
    def :=(value: Iterable[CssProperty]) = SelectorProperty(this, value)
  }

  case object div    extends ElementModelFactory[ElementScope | HTMLElementScope]("div")
  case object h1     extends ElementModelFactory[ElementScope | HTMLElementScope]("h1")
  case object button extends ElementModelFactory[ElementScope | HTMLElementScope | HTMLButtonElementScope]("button")

  case object disabled   extends WritableName[Boolean, HTMLButtonElementScope]
  case object classStyle extends WritableName[Iterable[CssProperty | SelectorProperty], HTMLElementScope]

  case object onclick extends EventName[MouseEvent, HTMLElementScope]

  case object backgroundColor extends CssName("background-color")

  case object _focus extends PseudoClass

  case object _disabled extends PseudoClass

  case object _hover extends PseudoClass

  case object _active extends PseudoClass

}
