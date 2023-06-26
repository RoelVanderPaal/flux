package flux

import flux.streams.{Observable, Subject, Subscriber, Subscription}
import org.scalajs.dom.*
import org.scalatest.BeforeAndAfterEach
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class RendererTest extends AnyFunSuite with Matchers with BeforeAndAfterEach {
  private def renderAndCheck[V: Viewable](viewable: V): Unit = {
    Renderer.render(document.body, viewable)
    check(viewable.toViews)
  }

  private def renderAndCheckObservable[V: Viewable](viewable: V): Unit = {
    Renderer.render(document.body, Observable.once(viewable))
    check(viewable.toViews)
  }
  private def check(elementChildren: View): Unit                       = check(List(elementChildren), document.body.childNodes)

  private def check(elementChildren: Iterable[View], childNodes: NodeList[Node] = document.body.childNodes): Unit = {
    childNodes.length shouldBe elementChildren.size
    childNodes
      .zip(elementChildren)
      .foreach((node, elementChild) => {
        (node, elementChild) match {
          case (e: Element, ElementModel(name, properties, children)) =>
            e.tagName shouldBe name.toUpperCase
            val attributes = properties.collect {
              case AttributeProperty(name, value: Boolean) if value                       => name -> ""
              case AttributeProperty(name, value: Any) if name != "key" && value != false => name -> value
            }.toMap
            attributes should contain theSameElementsAs e.attributes.view.mapValues(_.value).filterKeys(!_.startsWith("data-"))
            check(children, e.childNodes)
          case (t: Text, StringView(s))                               => t.wholeText shouldBe s
          case (c: Comment, EmptyNode)                                => c.data shouldBe "empty"
          case _                                                      => fail(s"no match $node $elementChild")
        }
      })
  }

  private val simpleElementModel = div(id := "test")(input(checked := true)())
  private val simpleText         = "text"
  test("simple ElementModel") { renderAndCheck(simpleElementModel) }
  test("simple ElementModel with style") { renderAndCheck(div(style := "color: #369;background-color: green")()) }
  test("simple ElementModel boolean true") { renderAndCheck(input(checked := true)()) }
  test("simple ElementModel boolean false") { renderAndCheck(input(checked := false)()) }
  test("simple String") { renderAndCheck(simpleText) }
  test("simple Empty") { renderAndCheck(EmptyNode) }
  test("simple list") { renderAndCheck(simpleText, simpleElementModel) }

  test("nested ElementModel") { renderAndCheck(div(id := "test")(span()("inner"))) }
  test("simple observable ElementModel") { renderAndCheckObservable(simpleElementModel) }
  test("simple observable String") { renderAndCheckObservable(simpleText) }
  test("simple observable Empty") { renderAndCheckObservable(EmptyNode) }
  ignore("simple observable list") {
//    val subject                    = Subject[Boolean]()
//    val r: Observable[Viewable[_]] = subject.map(s => if s then "true" else span()("false"))
//    Renderer.render(document.body, r)
//    Renderer.render(document.body, o)
//    subject.onNext(simpleElementModel)
//    renderAndCheck(subject)

//    val e1 = div(id := "id1", keyAlias := "k1", placeholder := "id1")("one")
//    val e2 = div(id := "id2", keyAlias := "k2", placeholder := "id2")("two")
//
//    val element1 = List(e1, e2)
//    subject.onNext(element1)
//    checkElementChild(element1)
//    val id1      = document.getElementById("id1")
//    val id2      = document.getElementById("id2")
//
//    def checkElement(ls: Iterable[NodeModel]) = {
//      subject.onNext(ls)
//      checkElementChild(ls)
//      document.getElementById("id1") shouldBe id1
//      document.getElementById("id2") shouldBe id2
//    }
//
//    checkElement(List("test", e2, e1))
//    checkElement(List(e2, "test", e1))
//    checkElement(List(e2, e1, "test"))
  }
  test("simple observable list in element") {
    val subject = Observable.once(1)

    def createView(c: Int) = List(strong()(c.toString), span()("item"))

    Renderer.render(document.body, div()(subject.map(createView)))

    check(div()(createView(1)))
  }

  override protected def beforeEach(): Unit = {
    document.body.replaceChildren()
  }

}
