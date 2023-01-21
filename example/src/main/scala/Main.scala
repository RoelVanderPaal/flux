import flux.streams.{Observable, Subject, Subscriber}
import flux.web.*
import org.scalajs.dom.{document, Event, MouseEvent}

case class Button(label: String) {
  val clicks = Subject[MouseEvent]()
  val value  = classStyle := Set(_hover := Set(backgroundColor := "green"))
  val view   = button(onclick := clicks, value, disabled := false)(label)
}
object Main extends App {
  val ticker = {
    val o = Observable.periodic(1000)

    val disabledSubject = Subject[MouseEvent]()

    val bplus         = Button("plus")
    val bplus10       = Button("plus 10")
    val bminus        = Button("minus")
    val disableButton = Button("disable")

    val dButton = button(disabled := disableButton.clicks.map(_ => 1).fold(0)(_ + _).map(_ % 2 == 0))("disabled?")

    val counter = Observable
      .merge(
        bplus.clicks.map(_ => 1),
        bplus10.clicks.map(_ => 10),
        bminus.clicks.map(_ => -1)
      )
      .fold(0)(_ + _)

    div(classStyle := List(backgroundColor := "red"))(bminus.view, counter, bplus.view, bplus10.view, disableButton.view, dButton)
//    o
  }

  Renderer.render(document.body, ticker)
}
