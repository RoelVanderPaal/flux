package flux

import flux.streams.constructor.AbstractObservable
import flux.streams.{Observable, Subscriber, Subscription}
import org.scalajs.dom.*

import scala.collection.AbstractIterable
import scala.reflect.TypeTest

object Renderer {
  def render(parent: Node, elementChildren: ElementChild*): Unit = render(parent, elementChildren, Iterable.empty[Node])
  def render(parent: Node, elementChildren: Iterable[ElementChild], existings: Iterable[Node]): Iterable[Result] =
    val ns = existings.map(Some(_)) ++ (existings.size until elementChildren.size).map(_ => None)
    existings.drop(elementChildren.size).foreach(parent.removeChild)
    elementChildren
      .zip(ns)
      .map { case (elementChild, existing) => renderElementChild(parent, elementChild, existing) }

  case class Result(node: Node, subscriptions: Iterable[Subscription] = Nil)

  def renderElementChild(parent: Node, elementChild: ElementChild, existing: Option[Node] = None): Result = {
    def replaceOrAppendChild[T <: Node](node: T, existing: Option[Node]): T = {
      existing match {
        case Some(e) => parent.replaceChild(node, e)
        case None    => parent.appendChild(node)
      }
      node
    }

    elementChild match {
      case ElementModel(name, properties, children)                   =>
        val element: Element = existing match {
          case Some(e: Element) if e.tagName == name.toUpperCase => e
          case _                                                 => replaceOrAppendChild(document.createElement(name), existing)
        }

        // attributes
        val attributeProperties = properties.collect { case ap: AttributeProperty[_, _] => ap }
        attributeProperties.foreach { case AttributeProperty(name, value) =>
          value match {
            case b: Boolean => if (b) element.setAttribute(name, "") else element.removeAttribute(name)
            case v          => element.setAttribute(name, value.toString)
          }
        }
        element.attributes.keySet
          .filterNot(_.startsWith("data-"))
          .diff(attributeProperties.map(_.name).toSet)
          .foreach(element.removeAttribute)

        val propertiesSubscriptions = properties.collect { case EventProperty(event, subscriber) =>
          Observable.fromEventListener(element, event).subscribe(subscriber)
        }
        properties.collect { case r: RefProperty[Element] => r }.foreach(_.subscriber.onNext(element))

        val results            = render(element, children, element.childNodes)
        val childSubscriptions = results.flatMap(_.subscriptions)
        Result(element, childSubscriptions ++ propertiesSubscriptions)
      case s: String                                                  =>
        Result(existing match {
          case Some(t: Text) =>
            t.data = s
            t
          case _             => replaceOrAppendChild(document.createTextNode(s), existing)
        })
      case EmptyNode                                                  =>
        Result(existing match {
          case Some(t: Comment) =>
            t.data = "empty"
            t
          case _                => replaceOrAppendChild(document.createComment("empty"), existing)
        })
      case o: Observable[AtomicModel | AbstractIterable[AtomicModel]] =>
        val element             = existing match {
          case Some(c: Comment) => c
          case _                => replaceOrAppendChild(document.createComment("placeholder"), existing)
        }
        val acc: Iterable[Node] = List(element)
        val subscription        = o
          .fold(Iterable(Result(element)))((a, v) =>
            a.flatMap(_.subscriptions).foreach(_.unsubscribe())
            render(
              parent,
              v match {
                case atomicModel: AtomicModel          => List(atomicModel)
                case as: AbstractIterable[AtomicModel] => as
              },
              a.map(_.node)
            )
          )
          .drain()
        Result(element, List(subscription))
    }
  }
}
