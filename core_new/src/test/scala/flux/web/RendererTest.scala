package flux.web

import flux.streams.{Observable, Subject}
import flux.web.key as keyAlias
import org.scalajs.dom.*
import org.scalatest.BeforeAndAfterEach
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class RendererTest extends AnyFunSuite with Matchers with BeforeAndAfterEach {

  def checkElementChild(element: ElementChild, childNodes: NodeList[Node] = document.body.childNodes): Unit = {
    element match {
      case es: Iterable[NodeModel] => check(childNodes, es.toList: _*)
      case _                       => check(childNodes, element)
    }
  }

  def check(childNodes: NodeList[Node], elementChildren: ElementChild*): Unit = {
    childNodes.length shouldBe elementChildren.length
    childNodes
      .zip(elementChildren)
      .foreach((node, elementChild) => {
        (node, elementChild) match {
          case (e: Element, ElementModel(name, properties, children)) =>
            e.tagName shouldBe name.toUpperCase
            val attributes = properties.collect {
              case AttributeProperty(name, value: String) if name != "key" => name -> value
              case AttributeProperty(name, value: Boolean) if value        => name -> ""
            }.toMap
            attributes should contain theSameElementsAs e.attributes.view.mapValues(_.value).filterKeys(!_.startsWith("data-"))
            check(e.childNodes, children.toSeq: _*)
          case (t: Text, s: String)                                   => t.wholeText shouldBe s
          case (c: Comment, EmptyNode)                                => c.data shouldBe "rest"
          case (c: Comment, _: Observable[Any])                       => c.data shouldBe "placeholder"
          case _                                                      => fail(s"no match $node $elementChild")
        }
      })
  }

  private def renderAndCheck(element: ElementChild): Unit = {
    Renderer.render(document.body, element)
    checkElementChild(element)
  }

  private def renderAndCheckObservable(element: SimpleElementChild): Unit = {
    Renderer.render(document.body, Observable.once(element))
    checkElementChild(element)
  }

  private val simpleElementModel = div(id := "test")(input(checked := true)())
  private val simpleText         = "text"
  test("simple ElementModel") { renderAndCheck(simpleElementModel) }
  test("simple ElementModel boolean true") { renderAndCheck(input(checked := true)()) }
  test("simple ElementModel boolean false") { renderAndCheck(input(checked := false)()) }
  test("simple String") { renderAndCheck(simpleText) }
  test("simple Empty") { renderAndCheck(EmptyNode) }
  test("simple list") { renderAndCheck(List(simpleText, simpleElementModel)) }
  test("list key") { renderAndCheck(List(simpleText, simpleElementModel)) }

  test("nested ElementModel") { renderAndCheck(div(id := "test")(span()("inner"))) }
  test("simple observable ElementModel") { renderAndCheckObservable(simpleElementModel) }
  test("simple observable String") { renderAndCheckObservable(simpleText) }
  test("simple observable Empty") { renderAndCheckObservable(EmptyNode) }
  test("simple observable list") {
    val subject = Subject[SimpleElementChild]()
    renderAndCheck(subject)

    val e1 = div(id := "id1", keyAlias := "k1", placeholder := "id1")("one")
    val e2 = div(id := "id2", keyAlias := "k2", placeholder := "id2")("two")

    val element1 = List(e1, e2)
    subject.onNext(element1)
    checkElementChild(element1)
    val id1      = document.getElementById("id1")
    val id2      = document.getElementById("id2")

    def checkElement(ls: Iterable[NodeModel]) = {
      subject.onNext(ls)
      checkElementChild(ls)
      document.getElementById("id1") shouldBe id1
      document.getElementById("id2") shouldBe id2
    }

    checkElement(List("test", e2, e1))
    checkElement(List(e2, "test", e1))
    checkElement(List(e2, e1, "test"))
  }

  test("replace String") {
    val subject = Subject[SimpleElementChild]()
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
    val subject = Subject[SimpleElementChild]()
    renderAndCheck(subject)
    subject.onNext(simpleElementModel)
    checkElementChild(simpleElementModel)
    val c1      = document.body.firstChild

    val simpleElementModel2 = div(placeholder := "placeholder")(input(checked := false)())
    subject.onNext(simpleElementModel2)
    checkElementChild(simpleElementModel2)
    document.body.firstChild shouldBe c1
  }

  private def createChild(label: String, children: ElementChild*): NodeModel = {
    val actions = Subject[NodeModel]()
    div()(
      button(id := "elementModel", onclick := actions.preProcess(_.mapTo(simpleElementModel)))("ElementModel"),
      button(id := "string", onclick := actions.preProcess(_.mapTo(simpleText)))("String"),
      button(id := "emptyNode", onclick := actions.preProcess(_.mapTo(EmptyNode)))("EmptyNode"),
      div(id := "children")((if (children.isEmpty) List(actions) else children): _*)
    )
  }

  def createParent(children: ElementChild*)(implicit childrenMap: Map[String, NodeModel]): NodeModel = {
    val parentActions = Subject[String]()
    div()(
      button(id := "one", onclick := parentActions.preProcess(_.mapTo("one")))("one"),
      button(id := "two", onclick := parentActions.preProcess(_.mapTo("two")))("two"),
      div()((if (children.isEmpty) List(parentActions.map(childrenMap(_))) else children): _*)
    )

  }

  test("complex children types") {
    implicit val childrenMap: Map[String, NodeModel] = List("one", "two").map(v => v -> createChild(v)).toMap

    val parent = createParent()
    Renderer.render(document.body, parent)
    check(document.body.childNodes, parent)
    document.getElementById("one").asInstanceOf[HTMLButtonElement].click()
    check(document.body.childNodes, createParent(createChild("one")))
    document.getElementById("elementModel").asInstanceOf[HTMLButtonElement].click()
    check(document.body.childNodes, createParent(createChild("one", simpleElementModel)))
    document.getElementById("string").asInstanceOf[HTMLButtonElement].click()
    check(document.body.childNodes, createParent(createChild("one", simpleText)))
    document.getElementById("emptyNode").asInstanceOf[HTMLButtonElement].click()
    check(document.body.childNodes, createParent(createChild("one", EmptyNode)))
    document.getElementById("one").asInstanceOf[HTMLButtonElement].click()
    check(document.body.childNodes, createParent(createChild("one", EmptyNode)))
  }
  test("complex counter") {
    def createCounter(label: String, count: Option[Int] = None): NodeModel = {
      val counts = Subject[Int]()
      val summed = counts.fold(0)(_ + _).startWith(0).remember()
      div()(
        span()(label),
        button(id := "+", onclick := counts.preProcess(_.mapTo(1)))("+"),
        count.map(_.toString).getOrElse(summed.map(_.toString))
      )
    }

    implicit val childrenMap: Map[String, NodeModel] = List("one", "two").map(v => v -> createCounter(v)).toMap
    val parent                                       = createParent()
    Renderer.render(document.body, parent)
    check(document.body.childNodes, parent)
    document.getElementById("one").asInstanceOf[HTMLButtonElement].click()
    document.getElementById("+").asInstanceOf[HTMLButtonElement].click()
    document.getElementById("one").asInstanceOf[HTMLButtonElement].click()
    document.getElementById("two").asInstanceOf[HTMLButtonElement].click()
    document.getElementById("one").asInstanceOf[HTMLButtonElement].click()
    check(document.body.childNodes, createParent(createCounter("one", Some(1))))
  }
  test("ref") {
    val inputRef = Subject[HTMLElement]()
    inputRef.subscribeNext(_.focus())

    val subject = Subject[SimpleElementChild]()
    renderAndCheck(subject)
    val element = div()(input(id := "input", ref := inputRef)())
    subject.onNext(element)
    check(document.body.childNodes, element)
    val i       = document.getElementById("input").asInstanceOf[HTMLInputElement]
    document.activeElement shouldBe i
    i.blur()
    document.activeElement should not be i

    val element2 = div()(input(id := "input")())
    subject.onNext(element2)
    check(document.body.childNodes, element2)
    document.activeElement should not be document.getElementById("input").asInstanceOf[HTMLInputElement]
  }

  // TODO check if not too many event listeners?

  override protected def beforeEach(): Unit = {
    document.body.replaceChildren()
  }
}
