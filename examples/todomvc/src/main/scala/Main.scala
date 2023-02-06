import flux.streams.{Observable, RememberSubject, Subject, Subscriber}
import flux.web.*
import org.scalajs.dom.{document, HTMLInputElement, KeyboardEvent, MouseEvent}

import java.util.UUID
import scala.scalajs.js.JSON
import scala.util.Random

type Key = String
def createKey(): Key = Random.nextDouble().toString
case class TodoItem(key: Key, completed: Boolean, label: String)
val INIT_TODOS       = List(
  TodoItem(createKey(), true, "Taste JavaScript"),
  TodoItem(createKey(), false, "Buy a unicorn")
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
  case SetCompletedAll(completed: Boolean)
  case ClearCompleted
  case Destroy(key: Key)
}
@main def main() = {
  val actions       = Subject[Action]()
  val enterPresseds = Subject[HTMLInputElement]()

  enterPresseds.subscribeNext(_.value = "")
  enterPresseds
    .map(_.value.trim())
    .map(Action.Add.apply)
    .subscribe(actions)

  val todos = actions
    .fold(INIT_TODOS)((a, v) =>
      v match {
        case Action.Add(v)                              => TodoItem(createKey(), false, v) :: a
        case Action.SetCompletedAll(completed: Boolean) => a.map(_.copy(completed = completed))
        case Action.ClearCompleted                      => a.filterNot(_.completed)
        case Action.Destroy(key)                        => a.filterNot(_.key == key)
      }
    )
    .startWith(INIT_TODOS)
    .remember()

//  todos.subscribeNext(println)

  val selectedFilter = RememberSubject[String]()
  val filters        = List("All", "Active", "Completed").map(Filter(selectedFilter))
  Observable
    .merge(filters.map(_.output): _*)
    .startWith("All")
    .subscribe(selectedFilter)

  val app = section(className := "todoapp")(
    header(className := "header")(
      h1("todos"),
      input(
        className   := "new-todo",
        placeholder := "What needs to be done?",
        autofocus   := true,
        onkeyup     := enterPresseds.preProcess(
          _.filter(_.keyCode == 13)
            .map(_.target.asInstanceOf[HTMLInputElement])
        )
      )()
    ),
    section(className := "main")(
      input(className := "toggle-all", `type` := "checkbox", checked := true)(),
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
                  button(className := "destroy", onclick := actions.preProcess(_.mapTo(Action.Destroy(t.key))))("")
                )
              )
            ): _*
          )
        )
    ),
    footer(className := "footer")(
      span(className := "todo-count")(strong(todos.map(_.filterNot(_.completed).length).text()), " item left"),
      ul(className := "filters")(filters.map(_.view): _*),
      button(className := "clear-completed", onclick := actions.preProcess(_.mapTo(Action.ClearCompleted)))("Clear completed")
    )
  )

  Renderer.render(document.body, app)

}
