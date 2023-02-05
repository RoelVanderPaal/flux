import flux.streams.{Observable, RememberSubject, Subject, Subscriber}
import flux.web.*
import org.scalajs.dom.{document, HTMLInputElement, KeyboardEvent, MouseEvent}

import scala.scalajs.js.JSON

case class TodoItem(completed: Boolean, label: String)
val INIT_TODOS = List(
  TodoItem(true, "Taste JavaScript"),
  TodoItem(false, "Buy a unicorn")
)
case class Filter(selected: Observable[String])(label: String) {
  private val clicks = Subject[MouseEvent]()
  val output         = clicks.mapTo(label)
  val view           = li()(
    a(onclick := clicks, className := selected.map(s => if (s == label) "selected" else ""), href := "#/")(label)
  )
}

enum Action {
  case Add(value: String)
}
@main def main() = {
  val selectedFilter = RememberSubject[String]()
  val inputs         = Subject[KeyboardEvent]()

  val enterPresseds = inputs
    .filter(_.keyCode == 13)
    .map(_.target.asInstanceOf[HTMLInputElement])
  enterPresseds.subscribeNext(_.value = "")

  val todos   = Observable
    .merge(
      enterPresseds
        .map(_.value.trim())
        .map(Action.Add.apply)
    )
    .fold(INIT_TODOS)((a, v) =>
      v match {
        case Action.Add(v) => TodoItem(false, v) :: a
      }
    )
    .startWith(INIT_TODOS)
    .remember()
  val filters = List("All", "Active", "Completed").map(Filter(selectedFilter))
  Observable
    .merge(filters.map(_.output): _*)
    .startWith("All")
    .subscribe(selectedFilter)
  val app     = section(className := "todoapp")(
    header(className := "header")(
      h1("todos"),
      input(className := "new-todo", placeholder := "What needs to be done?", autofocus := true, onkeyup := inputs)()
    ),
    section(className := "main")(
      input(className := "toggle-all", `type` := "checkbox")(),
      label()("Mark all as completed"),
      Observable
        .combine(todos, selectedFilter)
        .map { case (ts, f) =>
          f match {
            case "All"       => ts
            case "Active"    => ts.filterNot(_.completed)
            case "Completed" => ts.filter(_.completed)
          }
        }
        .map(ts =>
          ul(className := "todo-list")(
            ts.map(t =>
              li(className := (if (t.completed) "completed" else ""))(
                div(className := "view")(
                  input(className := "toggle", `type` := "checkbox", checked := t.completed)(),
                  label()(t.label),
                  button(className := "destroy")("")
                )
              )
            ): _*
          )
        )
    ),
    footer(className := "footer")(
      span(className := "todo-count")(strong(todos.map(_.filterNot(_.completed).length).text()), " item left"),
      ul(className := "filters")(filters.map(_.view): _*),
      button(className := "clear-completed")("Clear completed")
    )
  )

  Renderer.render(document.body, app)

}
