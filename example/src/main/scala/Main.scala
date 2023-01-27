import flux.streams.{Observable, Subject, Subscriber}
import flux.web.{given, *}
import org.scalajs.dom.{document, Event, MouseEvent}

case class Button(label: String) {
  val clicks = Subject[MouseEvent]()
  val value  = classStyle := Set(_hover := Set(backgroundColor := "green"), CssProperty.unsafe("padding", "10px"))
  val view   = button(Property.unsafeSubscriber("click", clicks), value, disabled := false)(label)
}

case class Display(label: String, o: Observable[Int]) {
  val view = div()(
    h1()(label),
    o
  )
}

case class Counter() {
  val bplus  = Button("plus")
  val bminus = Button("minus")

  val actions = Observable
    .merge(
      bplus.clicks.map(_ => 1),
      bminus.clicks.map(_ => -1)
    )
  val counter = actions
    .fold(0)(_ + _)
    .remember()

  val view = div(bplus.view, bminus.view, counter)
}

val unsafe = ElementModel.unsafe(
  "h2",
  List(classStyle := List(backgroundColor := "green", CssProperty.unsafe("color", "white"))),
  List("h2 test2")
)

object Main extends App {
  val ticker = {
    val o = Observable.periodic(1000).remember()

    val disableButton = Button("disable")
    val chooser       = disableButton.clicks.mapTo(1).fold(0)(_ + _).startWith(0).map(_ % 2 == 0).remember()

    val tab1          = Button("tab1")
    val tab2          = Button("tab2")
    val tabObservable = Observable.merge(tab1.clicks.mapTo("tab1"), tab2.clicks.mapTo("tab2")).startWith("tab1")

    val nested = div(classStyle := List(backgroundColor := "orange"))(
      button(disabled := chooser)("disabled?"),
      disableButton.view,
      div()(chooser.map(v => if (v) div(o) else "false"))
    )

    div()(tab1.view, tab2.view, tabObservable.map(t => if (t == "tab1") nested else "tab2"))
  }

  Renderer.render(document.body, ticker)
}
