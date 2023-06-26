package flux

import flux.streams.{Observable, Subject}
import org.scalajs.dom.*
import org.scalatest.BeforeAndAfterEach
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import scala.collection.AbstractIterable

class RendererTest extends AnyFunSuite with Matchers with BeforeAndAfterEach {
  def checkElementChild(elementChild: ElementChild) = checkElementChildren(List(elementChild))
  def checkElementChildren(elementChildren: Iterable[ElementChild], nodes: NodeList[Node] = document.body.childNodes): Unit = {
    nodes.length shouldBe elementChildren.size

    nodes
      .zip(elementChildren)
      .foreach((node, nodeModel) => {
        (node, nodeModel) match {
          case (e: Element, ElementModel(name, properties, children)) =>
            e.tagName shouldBe name.toUpperCase
            val attributes = properties.collect {
              case AttributeProperty(name, value: Boolean) if value                       => name -> ""
              case AttributeProperty(name, value: Any) if name != "key" && value != false => name -> value
            }.toMap
            attributes should contain theSameElementsAs e.attributes.view.mapValues(_.value).filterKeys(!_.startsWith("data-"))

            checkElementChildren(children, e.childNodes)
          case (t: Text, s: ElementModel)                             => fail(t.wholeText + " " + s)
          case (t: Text, s: String)                                   => t.wholeText shouldBe s
          case (c: Comment, EmptyNode)                                => c.data shouldBe "empty"
          case (c: Comment, o: Observable[_])                         => c.data shouldBe "placeholder"
          case _                                                      => fail(s"no match $node $nodeModel")
        }
      })
  }

  private def renderAndCheck(elementChildren: ElementChild*): Unit = {
    val parent = document.body
    Renderer.render(parent, elementChildren: _*)
    checkElementChildren(elementChildren)
  }

  private def renderAndCheckObservable(nodeModel: AtomicModel | AbstractIterable[AtomicModel]): Unit = {
    Renderer.render(document.body, Observable.once(nodeModel))
    val r = nodeModel match {
      case atomicModel: AtomicModel          => List(atomicModel)
      case ls: AbstractIterable[AtomicModel] => ls
    }
    checkElementChildren(r)
  }

  private val simpleElementModel = div(id := "test")(input(checked := true)())
  private val simpleText         = "text"
  test("simple ElementModel") { renderAndCheck(simpleElementModel) }
  test("simple String") { renderAndCheck(simpleText) }
  test("simple ElementModel with style") { renderAndCheck(div(style := "color: #369;background-color: green")()) }
  test("simple ElementModel boolean true") { renderAndCheck(input(checked := true)()) }
  test("simple ElementModel boolean false") { renderAndCheck(input(checked := false)()) }
  test("simple Empty") { renderAndCheck(EmptyNode) }
  test("simple list") { renderAndCheck(List(simpleText, simpleElementModel): _*) }
  test("simple observable ElementModel") { renderAndCheckObservable(simpleElementModel) }
  test("simple observable String") { renderAndCheckObservable(simpleText) }
  test("simple observable Empty") { renderAndCheckObservable(EmptyNode) }
  test("simple observable list") { renderAndCheckObservable(List(simpleElementModel, simpleText)) }
  test("simple observable list in element") {
    val subject = Subject[AtomicModel]()

    def createView(c: Int) = div(strong(c.toString), span("item"))
    Renderer.render(document.body, subject)
    subject.onNext(createView(1))
    checkElementChildren(List(createView(1)))
    subject.onNext(createView(2))
    checkElementChildren(List(createView(2)))
  }
  test("replace String") {
    val subject = Subject[AtomicModel]()
    renderAndCheck(subject)
    subject.onNext("test")
    val c1      = document.body.firstChild
    checkElementChild("test")
    subject.onNext("test2")
    checkElementChild("test2")
    document.body.firstChild shouldBe c1
    subject.onNext(simpleElementModel)
    checkElementChild(simpleElementModel)
    document.body.firstChild should not be c1
    subject.onNext("test")
    checkElementChild("test")
    document.body.firstChild should not be c1
  }
  test("remove attribute") {
    val subject = Subject[AtomicModel]()
    renderAndCheck(subject)
    subject.onNext(simpleElementModel)
    checkElementChild(simpleElementModel)
    val c1      = document.body.firstChild

    val simpleElementModel2 = div(placeholder := "placeholder")(input(checked := false)())
    subject.onNext(simpleElementModel2)
    checkElementChild(simpleElementModel2)
    document.body.firstChild shouldBe c1
  }

  private def createChild(label: String, children: ElementChild*): AtomicModel = {
    val actions = Subject[AtomicModel]()
    div()(
      button(id := "elementModel", onclick := actions.preProcess(_.mapTo(simpleElementModel)))("ElementModel"),
      button(id := "string", onclick := actions.preProcess(_.mapTo(simpleText)))("String"),
      button(id := "emptyNode", onclick := actions.preProcess(_.mapTo(EmptyNode)))("EmptyNode"),
      div(id := "children")((if (children.isEmpty) List(actions) else children): _*)
    )
  }

  def createParent(children: ElementChild*)(implicit childrenMap: Map[String, AtomicModel]): AtomicModel = {
    val parentActions = Subject[String]()
    div()(
      button(id := "one", onclick := parentActions.preProcess(_.mapTo("one")))("one"),
      button(id := "two", onclick := parentActions.preProcess(_.mapTo("two")))("two"),
      div()((if (children.isEmpty) List(parentActions.map(childrenMap(_))) else children): _*)
    )
  }

  test("complex children types") {
    implicit val childrenMap: Map[String, AtomicModel] = List("one", "two").map(v => v -> createChild(v)).toMap

    val parent = createParent()
    Renderer.render(document.body, parent)
    checkElementChild(parent)
    document.getElementById("one").asInstanceOf[HTMLButtonElement].click()
    checkElementChild(createParent(createChild("one")))
    document.getElementById("one").asInstanceOf[HTMLButtonElement].click()
    checkElementChild(createParent(createChild("one")))
    document.getElementById("elementModel").asInstanceOf[HTMLButtonElement].click()
    checkElementChild(createParent(createChild("one", simpleElementModel)))
    document.getElementById("string").asInstanceOf[HTMLButtonElement].click()
    checkElementChild(createParent(createChild("one", simpleText)))
    document.getElementById("emptyNode").asInstanceOf[HTMLButtonElement].click()
    checkElementChild(createParent(createChild("one", EmptyNode)))
    document.getElementById("one").asInstanceOf[HTMLButtonElement].click()
    checkElementChild(createParent(createChild("one", EmptyNode)))
  }
  test("complex counter") {
    def createCounter(label: String, count: Option[Int] = None): AtomicModel = {
      val counts = Subject[Int]()
      val summed = counts
        .fold(0)(_ + _)
        .startWith(0)
        .remember()
      div()(
        span()(label),
        button(id := "+", onclick := counts.preProcess(_.mapTo(1)))("+"),
        count.map(_.toString).getOrElse(summed.map(_.toString))
      )
    }

    implicit val childrenMap: Map[String, AtomicModel] = List("one", "two").map(v => v -> createCounter(v)).toMap
    val parent                                         = createParent()
    Renderer.render(document.body, parent)
    checkElementChild(parent)
    document.getElementById("one").asInstanceOf[HTMLButtonElement].click()
    checkElementChild(createParent(createCounter("one", Some(0))))
    document.getElementById("+").asInstanceOf[HTMLButtonElement].click()
    document.getElementById("one").asInstanceOf[HTMLButtonElement].click()
    checkElementChild(createParent(createCounter("one", Some(1))))
    document.getElementById("two").asInstanceOf[HTMLButtonElement].click()
    document.getElementById("one").asInstanceOf[HTMLButtonElement].click()
    checkElementChild(createParent(createCounter("one", Some(1))))
  }
  test("ref") {
    val inputRef = Subject[HTMLElement]()
    inputRef.subscribeNext(_.focus())

    val subject = Subject[AtomicModel]()
    renderAndCheck(subject)
    val element = div()(input(id := "input", ref := inputRef)())
    subject.onNext(element)
    checkElementChild(element)
    val i       = document.getElementById("input").asInstanceOf[HTMLInputElement]
    document.activeElement shouldBe i
    i.blur()
    document.activeElement should not be i

    val element2 = div()(input(id := "input")())
    subject.onNext(element2)
    checkElementChild(element2)
    document.activeElement should not be document.getElementById("input").asInstanceOf[HTMLInputElement]
  }

  override protected def beforeEach(): Unit = {
    document.body.replaceChildren()
  }
}
