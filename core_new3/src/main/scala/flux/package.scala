import flux.streams.constructor.AbstractObservable
import flux.streams.{Observable, Subscriber}
import org.scalajs.dom.*

import scala.collection.AbstractIterable
import scala.collection.immutable.AbstractSeq

package object flux {
  type AtomicModel  = String | ElementModel[_] | EmptyNode.type
  type ElementChild = AtomicModel | Observable[AtomicModel | AbstractIterable[AtomicModel]]

  case object EmptyNode

  trait Property[-T <: Element]
  case class PropertyProperty[T <: Element, V](setter: (T, V) => Unit, value: V)                            extends Property[T]
  case class ObservablePropertyProperty[T <: Element, V](setter: (T, V) => Unit, observable: Observable[V]) extends Property[T]
  case class AttributeProperty[T <: Element, V](name: String, value: V)                                     extends Property[T]
  case class ObservableAttributeProperty[T <: Element, V](name: String, observable: Observable[V])          extends Property[T]

  case class EventProperty[T <: Element, V <: Event](event: String, v: Subscriber[V]) extends Property[T]
  case class RefProperty[T <: Element](subscriber: Subscriber[T])                     extends Property[T]
  case class DataProperty[T <: Element](m: Map[String, String])                       extends Property[T]
  trait AttributeName[T <: Element, V](name: String)       {
    def :=(v: V)             = AttributeProperty[T, V](name, v)
    def :=(v: Observable[V]) = ObservableAttributeProperty[T, V](name, v)
  }
  trait EventName[T <: Element, V <: Event](event: String) {
    def :=(v: Subscriber[V]) = EventProperty[T, V](event, v)
  }

  case class ElementModel[T <: Element](name: String, properties: Iterable[Property[T]], children: Iterable[ElementChild])

  trait ElementModelFactory[T <: Element](name: String) {
    def apply(children: ElementChild*): ElementModel[T]                        = ElementModel(name, Nil, children)
    def apply(properties: Property[T]*): ElementModelFactoryWithoutChildren[T] = ElementModelFactoryWithoutChildren[T](name, properties)
  }

  class ElementModelFactoryWithoutChildren[T <: Element](name: String, properties: Iterable[Property[T]]) {
    def apply(children: ElementChild*): ElementModel[T] = ElementModel(name, properties, children)
  }

  trait PropertyName[T <: Element, V](setter: (T, V) => Unit) {
    def :=(v: V)             = PropertyProperty[T, V](setter, v)
    def :=(v: Observable[V]) = ObservablePropertyProperty[T, V](setter, v)
  }

  case object ul      extends ElementModelFactory[HTMLElement]("ul")
  case object li      extends ElementModelFactory[HTMLElement]("li")
  case object label   extends ElementModelFactory[HTMLElement]("label")
  case object section extends ElementModelFactory[HTMLElement]("section")
  case object header  extends ElementModelFactory[HTMLElement]("header")
  case object h1      extends ElementModelFactory[HTMLElement]("h1")
  case object a       extends ElementModelFactory[HTMLAnchorElement]("a")
  case object strong  extends ElementModelFactory[HTMLElement]("strong")
  case object span    extends ElementModelFactory[HTMLElement]("span")
  case object footer  extends ElementModelFactory[HTMLElement]("footer")
  case object div     extends ElementModelFactory[HTMLDivElement]("div")
  case object input   extends ElementModelFactory[HTMLInputElement]("input")
  case object button  extends ElementModelFactory[HTMLButtonElement]("button")

  case object href    extends AttributeName[HTMLAnchorElement, String]("href")
  case object checked extends PropertyName[HTMLInputElement, Boolean]((e, v) => e.checked = v)

  case object className   extends AttributeName[Element, String]("class")
  case object autofocus   extends AttributeName[HTMLElement, Boolean]("autofocus")
  case object id          extends AttributeName[Element, String]("id")
  case object `for`       extends AttributeName[Element, String]("for")
  case object placeholder extends AttributeName[Element, String]("placeholder")
  case object `type`      extends AttributeName[Element, String]("type")
  case object value       extends AttributeName[Element, String]("value")
  case object style       extends AttributeName[Element, String]("style")
  case object ariaLabel   extends AttributeName[Element, String]("aria-label")
  case object disabled    extends AttributeName[HTMLInputElement | HTMLButtonElement, Boolean]("disabled")

  case object ref  {
    def :=[T <: Element](s: Subscriber[T]) = RefProperty[T](s)
  }
  case object data {
    def :=[T <: Element](m: Map[String, String]) = DataProperty[T](m)
  }

  case object onchange   extends EventName[HTMLElement, Event]("change")
  case object onkeydown  extends EventName[HTMLElement, KeyboardEvent]("keydown")
  case object onclick    extends EventName[HTMLElement, MouseEvent]("click")
  case object ondblclick extends EventName[HTMLElement, MouseEvent]("dblclick")
  case object onblur     extends EventName[HTMLElement, FocusEvent]("blur")
  case object onkeyup    extends EventName[HTMLElement, KeyboardEvent]("keyup")

}
