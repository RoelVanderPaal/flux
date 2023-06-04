package flux.web

import flux.streams.{Observable, Subject}
import org.scalajs.dom.*
import org.scalatest.BeforeAndAfterEach
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class RendererTest extends AnyFunSuite with Matchers with BeforeAndAfterEach {
  def check(childNodes: NodeList[Node], elementChildren: ElementChild*): Unit = {
    childNodes.length shouldBe elementChildren.length
    childNodes
      .zip(elementChildren)
      .foreach((node, elementChild) => {
        (node, elementChild) match {
          case (e: Element, ElementModel(name, properties, children)) =>
            e.tagName shouldBe name.toUpperCase
            val attributes = properties.collect {
              case AttributeProperty(name, value: String)           => name -> value
              case AttributeProperty(name, value: Boolean) if value => name -> ""
            }.toMap
            attributes should contain theSameElementsAs e.attributes.view.mapValues(_.value)
            check(e.childNodes, children.toSeq: _*)
          case (t: Text, s: String)                                   => t.wholeText shouldBe s
          case (c: Comment, EmptyNode)                                => c.data shouldBe "rest"
          case (c: Comment, s: Observable[NodeModel])                 => c.data shouldBe "placeholder"
          case _                                                      => fail(s"no match $node $elementChild")
        }
      })
  }

  private def renderAndCheck(element: ElementChild): Unit = {
    Renderer.render(document.body, element)
    check(document.body.childNodes, element)
  }

  private def renderAndCheckObservable(element: NodeModel): Unit = {
    Renderer.render(document.body, Observable.once(element))
    check(document.body.childNodes, element)
  }

  private val simpleElementModel = div(id := "test")(input()())
  private val simpleText         = "text"
  test("simple ElementModel") { renderAndCheck(simpleElementModel) }
  test("simple ElementModel boolean true") { renderAndCheck(input(checked := true)()) }
  test("simple ElementModel boolean false") { renderAndCheck(input(checked := false)()) }
  test("simple String") { renderAndCheck(simpleText) }
  test("simple Empty") { renderAndCheck(EmptyNode) }
  test("nested ElementModel") { renderAndCheck(div(id := "test")(span()("inner"))) }
  test("simple observable ElementModel") { renderAndCheckObservable(simpleElementModel) }
  test("simple observable String") { renderAndCheckObservable(simpleText) }
  test("simple observable Empty") { renderAndCheckObservable(EmptyNode) }

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
    check(document.body.childNodes, createParent(createChild("one")))
  }
  test("complex counter") {
    def createCounter(label: String, count: Option[Int] = None): NodeModel = {
      println(s"createCounter $label $count")
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

    val element = div()(input(id := "input", ref := inputRef)())
    Renderer.render(document.body, Observable.once(element))
    check(document.body.childNodes, element)
    val i       = document.getElementById("input").asInstanceOf[HTMLInputElement]
    document.activeElement shouldBe i
  }

  // TODO check if not too many event listeners?

  override protected def beforeEach(): Unit = {
    document.body.replaceChildren()
  }
}
