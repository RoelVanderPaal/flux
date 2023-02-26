package flux
import flux.streams.{Observable, Subscriber}
import org.scalajs.dom.*

import scala.language.implicitConversions

package object web {

  case object EmptyNode
  type NodeModel    = String | ElementModel | EmptyNode.type
  type ElementChild = NodeModel | Observable[NodeModel]

  private case class ElementModel(name: String, properties: Iterable[Property[_, _]], children: Iterable[ElementChild])

  private trait Scope
  private trait ElementScope           extends Scope
  private trait HTMLElementScope       extends Scope
  private trait HTMLButtonElementScope extends Scope
  private trait HTMLInputElementScope  extends Scope

  sealed private trait Property[Value, +Scope] {
    def key: String
  }
  object Property                              {
    def unsafe[Value](key: String, v: Value)                       = AttributeProperty(key, v)
    def unsafeObservable[Value](key: String, v: Observable[Value]) = ObservableProperty(key, v)
    def unsafeSubscriber[Value](key: String, v: Subscriber[Value]) = SubscriberProperty(key, v)

  }

  // TODO get rid of Value
  private case class AttributeProperty[Value, Scope](key: String, value: Value)                       extends Property[Value, Scope]
  // TODO merge with SimpleProperty
  private case class ObservableProperty[Value, Scope](key: String, value: Observable[Value])          extends Property[Value, Scope]
  private case class SubscriberProperty[Value <: Event, Scope](key: String, value: Subscriber[Value]) extends Property[Value, Scope]
  private case class RefProperty[Value <: Element](value: Subscriber[Value]) extends Property[Value, ElementScope] {
    def key = "ref"
  }
  case class Ref[Value <: Element](value: Subscriber[Value])

  sealed private trait Name[Value, +Scope] {
    def name: String
  }

  private trait AttributeName[Value, Scope] extends Name[Value, Scope] {
    def :=(value: Value)             = AttributeProperty[Value, Scope](this.name, value)
    def :=(value: Observable[Value]) = ObservableProperty[Value, Scope](this.name, value)

    override def name: String = this.toString
  }

  private trait EventName[Value <: Event, Scope] extends Name[Value, Scope] {
    def :=(value: Subscriber[Value]) = SubscriberProperty(this.name, value)

    override def name: String = this.toString.stripPrefix("on")
  }
  private trait MethodName[Value, Scope]         extends Name[Value, Scope] {
    override def name: String = this.toString
  }

  object ElementModel {
    def unsafe(name: String, properties: Seq[Property[_, _]], children: Iterable[ElementChild]) = ElementModel(name, properties, children)
  }

  private class ElementModelFactoryWithoutChildren(name: String, properties: Iterable[Property[_, _]]) {
    def apply(children: ElementChild*): ElementModel = ElementModel(name, properties, children)
  }

  private trait ElementModelFactory[S <: Scope, T <: Element](name: String) {
    def apply(children: ElementChild*): ElementModel = ElementModel(name, List.empty[Property[_, S]], children)

    def apply(properties: (Property[_, S] | Ref[T])*) = new ElementModelFactoryWithoutChildren(
      name,
      properties.map {
        case p: Property[_, _] => p
        case r: Ref[_]         => RefProperty(r.value)
      }
    )
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

  abstract private class ElementFactory(name: String) extends ElementModelFactory[ElementScope | HTMLElementScope, HTMLElement](name)
  abstract private class InputElementFactory(name: String)
      extends ElementModelFactory[ElementScope | HTMLElementScope | HTMLInputElementScope, HTMLInputElement](name)
  abstract private class ButtonElementFactory(name: String)
      extends ElementModelFactory[ElementScope | HTMLElementScope | HTMLButtonElementScope, HTMLButtonElement](name)

  case object div     extends ElementFactory("div")
  case object section extends ElementFactory("section")
  case object header  extends ElementFactory("header")
  case object footer  extends ElementFactory("footer")
  case object h1      extends ElementFactory("h1")
  case object label   extends ElementFactory("label")
  case object span    extends ElementFactory("span")
  case object strong  extends ElementFactory("strong")
  case object ul      extends ElementFactory("ul")
  case object li      extends ElementFactory("li")
  case object a       extends ElementFactory("a")
  case object input   extends InputElementFactory("input")
  case object button  extends ButtonElementFactory("button")

  case object disabled    extends AttributeName[Boolean, HTMLButtonElementScope & HTMLInputElementScope]
  case object checked     extends AttributeName[Boolean, HTMLInputElementScope]
  case object autofocus   extends AttributeName[Boolean, HTMLElementScope]
  case object className   extends AttributeName[String, ElementScope]
  case object id          extends AttributeName[String, ElementScope]
  case object `for`       extends AttributeName[String, ElementScope]
  case object href        extends AttributeName[String, ElementScope]
  case object placeholder extends AttributeName[String, ElementScope]
  case object `type`      extends AttributeName[String, ElementScope]
  case object value       extends AttributeName[String, ElementScope]
  case object classStyle  extends AttributeName[Iterable[CssProperty | SelectorProperty], HTMLElementScope]

  case object onchange   extends EventName[MouseEvent, HTMLElementScope]
  case object onclick    extends EventName[MouseEvent, HTMLElementScope]
  case object ondblclick extends EventName[MouseEvent, HTMLElementScope]
  case object onkeyup    extends EventName[KeyboardEvent, HTMLElementScope]
  case object onblur     extends EventName[FocusEvent, HTMLElementScope]
  case object focus      extends MethodName[Unit, HTMLElementScope]

  case object backgroundColor extends CssName("background-color")

  case object _focus extends PseudoClass

  case object _disabled extends PseudoClass

  case object _hover extends PseudoClass

  case object _active extends PseudoClass

}
