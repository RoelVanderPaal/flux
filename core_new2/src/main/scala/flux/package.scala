import flux.streams.Observable
import org.scalajs.dom.*

package object flux {
  sealed trait View

  trait Viewable[T] {
    extension (x: T) {
      def toViews: Iterable[View]
    }
  }

  case class StringView(s: String) extends View

  given Viewable[String] with {
    extension (x: String) {
      def toViews: Iterable[View] = List(StringView(x))
    }
  }

  case class ObservableView(s: Observable[Iterable[View]]) extends View
  given [A: Viewable]: Viewable[Observable[A]] with {
    extension (o: Observable[A]) {
      def toViews: Iterable[View] = List(ObservableView(o.map(_.toViews)))
    }
  }

  given Viewable[EmptyTuple] with {
    extension (x: EmptyTuple) {
      def toViews: Iterable[View] = Nil
    }
  }

  given [A: Viewable]: Viewable[List[A]] with {
    extension (xs: List[A]) {
      def toViews: Iterable[View] = xs.flatMap(_.toViews)
    }
  }

  given [A: Viewable, B <: Tuple: Viewable]: Viewable[A *: B] with {
    extension (x: A *: B) {
      def toViews: Iterable[View] = x.head.toViews ++ x.tail.toViews
    }
  }

  given Viewable[ElementModel] with   {
    extension (x: ElementModel) {
      def toViews: Iterable[View] = List(x)
    }
  }
  case object EmptyNode extends View
  given Viewable[EmptyNode.type] with {
    extension (x: EmptyNode.type) {
      def toViews: Iterable[View] = List(x)
    }
  }

  trait Property[-T]
  case class AttributeProperty[T, V](name: String, value: V) extends Property[T]
  trait AttributeName[T, V](name: String) {
    def :=(v: V) = AttributeProperty[T, V](name, v)
//    def :=(v: Observable[V]) = ObservableAttributeProperty[T, V](name, v)
  }

  case class ElementModel(name: String, properties: Iterable[Property[_]], children: Iterable[View]) extends View
  trait ElementModelFactory[T <: Element](name: String)                                     {
    def apply(properties: Property[T]*): ElementModelFactoryWithoutChildren = ElementModelFactoryWithoutChildren(name, properties)
//    def apply[V: Viewable](children: V = EmptyTuple): ElementModel          = ElementModel(name, Nil, children.toViews)
  }
  class ElementModelFactoryWithoutChildren(name: String, properties: Iterable[Property[_]]) {
    def apply[V: Viewable](children: V = EmptyTuple): ElementModel = ElementModel(name, properties, children.toViews)
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
  case object style       extends AttributeName[Element, String]("style")
  case object ariaLabel   extends AttributeName[Element, String]("aria-label")
  case object disabled    extends AttributeName[HTMLInputElement | HTMLButtonElement, Boolean]("disabled")

}
