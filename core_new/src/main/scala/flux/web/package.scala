package flux

import flux.streams.{Observable, Subscriber, Subscription}
import flux.web.AttributeName
import org.scalajs.dom.*

package object web {
  trait Property[-T]
  case class AttributeProperty[T, V](name: String, value: V)                            extends Property[T]
  case class ObservableAttributeProperty[T, V](name: String, observable: Observable[V]) extends Property[T]
  case class RefProperty[T <: Element](subscriber: Subscriber[T])                       extends Property[T]
  case class KeyProperty[T <: Element, V](value: V)                                     extends Property[T]
  case class OnComponentUpdateProperty[T <: Element](subscriber: Subscriber[T])         extends Property[T]
  case class EventProperty[T, V <: Event](event: String, v: Subscriber[V])              extends Property[T]
  trait AttributeName[T, V](name: String)       {
    def :=(v: V)             = AttributeProperty[T, V](name, v)
    def :=(v: Observable[V]) = ObservableAttributeProperty[T, V](name, v)
  }
  trait EventName[T, V <: Event](event: String) {
    def :=(v: Subscriber[V]) = EventProperty[T, V](event, v)
  }

  case object EmptyNode

  type NodeModel          = String | ElementModel[_] | EmptyNode.type
  type SimpleElementChild = NodeModel | Iterable[NodeModel]
  type ElementChild       = SimpleElementChild | Observable[SimpleElementChild]

  case class ElementModel[T <: Element](name: String, properties: Iterable[Property[T]], children: Iterable[ElementChild])

  trait ElementModelFactory[T <: Element](name: String)                                                   {
    def apply(children: ElementChild*): ElementModel[T] = ElementModel(name, Nil, children)
    def apply(properties: Property[T]*)                 = ElementModelFactoryWithoutChildren[T](name, properties)
  }
  class ElementModelFactoryWithoutChildren[T <: Element](name: String, properties: Iterable[Property[T]]) {

    def apply(children: ElementChild*): ElementModel[T] = ElementModel(name, properties, children)
  }

  case object ul      extends ElementModelFactory[HTMLElement]("ul")
  case object li      extends ElementModelFactory[HTMLElement]("li")
  case object label   extends ElementModelFactory[HTMLElement]("label")
  case object section extends ElementModelFactory[HTMLElement]("section")
  case object header  extends ElementModelFactory[HTMLElement]("header")
  case object h1      extends ElementModelFactory[HTMLElement]("h1")
  case object a       extends ElementModelFactory[HTMLElement]("a")
  case object strong  extends ElementModelFactory[HTMLElement]("strong")
  case object span    extends ElementModelFactory[HTMLElement]("span")
  case object footer  extends ElementModelFactory[HTMLElement]("footer")
  case object div     extends ElementModelFactory[HTMLDivElement]("div")
  case object input   extends ElementModelFactory[HTMLInputElement]("input")
  case object button  extends ElementModelFactory[HTMLButtonElement]("button")

  case object href        extends AttributeName[Element, String]("href")
  case object checked     extends AttributeName[HTMLInputElement, Boolean]("checked")
  case object className   extends AttributeName[Element, String]("class")
  case object autofocus   extends AttributeName[HTMLElement, Boolean]("autofocus")
  case object id          extends AttributeName[Element, String]("id")
  case object `for`       extends AttributeName[Element, String]("for")
  case object placeholder extends AttributeName[Element, String]("placeholder")
  case object `type`      extends AttributeName[Element, String]("type")
  case object value       extends AttributeName[Element, String]("value")

  case object disabled extends AttributeName[HTMLInputElement | HTMLButtonElement, Boolean]("disabled")
  case object ref {
    def :=[T <: Element](s: Subscriber[T]) = RefProperty[T](s)
  }
  case object key {
    def :=[T <: Element, V](value: V) = KeyProperty[T, V](value)
  }

  case object onComponentUpdate {
    def :=[T <: Element](s: Subscriber[T]) = OnComponentUpdateProperty[T](s)
  }

  case object onchange   extends EventName[HTMLElement, Event]("change")
  case object onkeydown  extends EventName[HTMLElement, KeyboardEvent]("keydown")
  case object onclick    extends EventName[HTMLElement, MouseEvent]("click")
  case object ondblclick extends EventName[HTMLElement, MouseEvent]("dblclick")
  case object onblur     extends EventName[HTMLElement, FocusEvent]("blur")
  case object onkeyup    extends EventName[HTMLElement, KeyboardEvent]("keyup")
}
