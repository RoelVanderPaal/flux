package flux.web

import flux.streams.operators.{QueueSubscriber, QueuedOperator}
import flux.streams.{Observable, Subject, Subscriber, Subscription}
import org.scalajs.dom.*

import scala.reflect.ClassTag

object Renderer {
  def render(parent: Node, elementChild: ElementChild): Unit = {
    renderInternal(parent, elementChild)
  }

  case class Result(node: Node, subscriptions: Iterable[Subscription] = Nil)
  case class ManyResult(nodes: Iterable[Node], subscriptions: Iterable[Subscription] = Nil)

  case class NodeWithKey(node: Node, key: Option[String])
  def renderInternalMany(
    parent: Node,
    elementChilds: Iterable[NodeModel],
    existing: Iterable[Node]
  ) = {
    val nodeWithKeys              = existing.map(node =>
      NodeWithKey(
        node,
        node match {
          case h: HTMLElement => h.dataset.get(DATA_KEY_NAME)
          case _              => None
        }
      )
    )
    val (nodeWithKeysRest, nodes) = elementChilds.foldLeft((nodeWithKeys, List.empty[Option[Node]])) { case ((nodeWithKeys, l), e) =>
      nodeWithKeys match {
        case nodeWithKey :: rest =>
          val elementKey = e match {
            case ElementModel(_, properties, _) => properties.collectFirst { case k: KeyProperty[_, _] => k.value.toString }
            case _                              => None
          }

          def replace(n: NodeWithKey) = {
            parent.insertBefore(n.node, nodeWithKey.node)
            (nodeWithKeys.filter(_ != n), Some(n.node) :: l)
          }

          def useFirstWithoutKey = rest.find(_.key.isEmpty) match {
            case Some(n) => replace(n)
            case None    => replace(NodeWithKey(document.createComment("placeholder"), None))
          }

          (elementKey, nodeWithKey.key) match {
            case (Some(ek), nk) if !nk.contains(ek) =>
              rest.find(_.key.exists(_ == ek)) match {
                case Some(n) => replace(n)
                case None    => useFirstWithoutKey
              }
            case (None, Some(_))                    => useFirstWithoutKey
            case _                                  => (rest, Some(nodeWithKey.node) :: l)

          }
        case _                   => (Nil, None :: l)
      }
    }
    nodeWithKeysRest.map(_.node).foreach(parent.removeChild)
    val results                   = elementChilds
      .zip(nodes.reverse)
      .map((e, existing) => renderInternal(parent, e, existing))
    ManyResult(results.map(_.node), results.flatMap(_.subscriptions))
  }

  def renderInternal(
    parent: Node,
    elementChild: ElementChild,
    existing: Option[Node] = None
  ): Result = {
    def replaceOrAppendChild[T <: Node](node: T, existing: Option[Node]) = {
      existing match {
        case Some(e) => parent.replaceChild(node, e)
        case None    => parent.appendChild(node)
      }
      node
    }

    elementChild match {
      case ElementModel(name, properties, children) =>
        val updates                                = Subject[Element]()
        val element                                = existing match {
          case Some(e: Element) if e.tagName == name.toUpperCase => e
          case _                                                 => replaceOrAppendChild(document.createElement(name), existing)
        }
        // attributes
        def setAttribute(name: String, value: Any) = value match {
          case b: Boolean => if (b) element.setAttribute(name, "") else element.removeAttribute(name)
          case v          => element.setAttribute(name, value.toString)
        }
        val attributeProperties                    = properties.collect { case ap: AttributeProperty[_, _] => ap }
        attributeProperties.foreach { case AttributeProperty(name, value) => setAttribute(name, value) }
        element.attributes.keySet
          .filterNot(_.startsWith("data-"))
          .diff(attributeProperties.map(_.name).toSet)
          .foreach(element.removeAttribute)
        // ref
        properties.collect { case r: RefProperty[Element] => r }.foreach(_.subscriber.onNext(element))
        element match {
          case h: HTMLElement =>
            h.dataset.empty
            properties
              .collect { case DataProperty(m) => m }
              .foreach(_.map { case (name, value) => h.dataset.put(name, value) })
        }
        properties
          .collect { case k: KeyProperty[Element, _] => k }
          .map(_.value.toString)
          .foreach(value =>
            element match {
              case h: HTMLElement => h.dataset.put(DATA_KEY_NAME, value)
            }
          )

        val propertiesSubscriptions = properties.collect {
          case ObservableAttributeProperty(name, observable)  =>
            observable
              .subscribeNext(value => {
                setAttribute(name, value)
                updates.onNext(element)
              })
          case EventProperty(event, subscriber)               => Observable.fromEventListener(element, event).subscribe(subscriber)
          case OnComponentUpdateProperty[Element](subscriber) => updates.subscribe(subscriber)
        }

        val childNodes                            = element.childNodes
        val subscriptions: Iterable[Subscription] =
          children
            .zip(childNodes.map(Some(_)) ++ (1 to (children.size - element.childNodes.length)).map(_ => None))
            .map((e, n) => renderInternal(element, e, n))
            .flatMap(_.subscriptions)
            ++ propertiesSubscriptions
        if (existing.isDefined) {
          childNodes.drop(children.size).foreach(element.removeChild)
        }

        updates.onNext(element)
        Result(element, subscriptions)
      case s: String                                =>
        Result(existing match {
          case Some(t: Text) =>
            t.data = s
            t
          case _             => replaceOrAppendChild(document.createTextNode(s), existing)
        })
//      case o: Observable[_]                         =>
//        println("en niet hier")
//        val node         = document.createComment("placeholder")
//        parent.appendChild(node)
//        val subscription = o
//          .fold(Result(node))((existing, e) => {
//            existing.subscriptions.foreach(_.unsubscribe())
//            renderInternal(parent, e, Some(existing.node))
//          })
//          .subscribe(unitSubscriber[Result])
//        Result(node, List(subscription))
//      case o: Observable[Iterable[NodeModel]]       =>
//        println("hier")
//        val node         = document.createComment("placeholder")
//        parent.appendChild(node)
//        val subscription = o
//          .fold(ManyResult(List(node)))((existing, e) => {
//            existing.subscriptions.foreach(_.unsubscribe())
//            renderInternalMany(parent, e, existing.nodes)
//          })
//          .subscribe(unitSubscriber[ManyResult])
//        Result(node, List(subscription))
      case o: Observable[Any]                       =>
        Result(parent, Nil)
        o.subscribe(new Subscriber[Any] {
          override def onNext(t: Any): Unit = t match {
            case l: List[_] =>
            case _          =>
          }

          override def onCompleted: Unit = ???
        })
        Result(parent, Nil)
      case _                                        => Result(replaceOrAppendChild(document.createComment("rest"), existing))
    }
  }
  val DATA_KEY_NAME     = "fluxkey"
  def unitSubscriber[T] = new Subscriber[T] {
    override def onNext(t: T): Unit = {}

    override def onCompleted: Unit = {}
  }
}
