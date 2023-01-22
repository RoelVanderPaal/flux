import flux.streams.{Observable, Subject, Subscriber}
import flux.web.*
import org.scalajs.dom.{document, Event, MouseEvent}

case class Button(label: String) {
  val clicks = Subject[MouseEvent]()
  val value  = classStyle := Set(_hover := Set(backgroundColor := "green"))
  val view   = button(Property.unsafeSubscriber("click", clicks), value, disabled := false)(label)
}

case class Display(label: String, o: Observable[Int]) {
  val view = div()(
    h1()(label),
    o
  )
}
object Main extends App {
  val ticker = {
    val o = Observable.periodic(1000)

    val disabledSubject = Subject[MouseEvent]()

    val bplus         = Button("plus")
    val bplus10       = Button("plus 10")
    val bminus        = Button("minus")
    val disableButton = Button("disable")

    val chooser = disableButton.clicks.map(_ => 1).fold(0)(_ + _).map(_ % 2 == 0)
    val dButton = button(disabled := chooser)("disabled?")

    val actions = Observable
      .merge(
        bplus.clicks.map(_ => 1),
        bplus10.clicks.map(_ => 10),
        bminus.clicks.map(_ => -1)
      )
    val counter = actions
      .fold(0)(_ + _)
      .remember()

    div(classStyle := List(backgroundColor := "orange"))(
      bminus.view,
//      counter,
      bplus.view,
//      bplus10.view,
//      ElementModel.unsafe(
//        "h2",
//        List(classStyle := List(backgroundColor := "green", CssProperty.unsafe("color", "white"))),
//        List("h2 test2")
//      ),
      disableButton.view,
//      dButton,
      div()(chooser.map(v => if (v) counter else h1("false")))
    )
//    div()(o)
  }

  Renderer.render(document.body, ticker)
}
