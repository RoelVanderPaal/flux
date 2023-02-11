import flux.streams.{Observable, RememberSubject, Subject, Subscriber}
import flux.web.*
import org.scalajs.dom.*

import java.util.UUID
import scala.scalajs.js
import scala.scalajs.js.JSON
import scala.scalajs.js.annotation.JSExportAll
import scala.util.Random

type Key = String
def createKey(): Key = Random.nextDouble().toString
case class Filter(selected: Observable[String])(label: String) {
  private val clicks = Subject[MouseEvent]()
  val output         = clicks.mapTo(label)
  val view           = li()(
    a(onclick := clicks, className := selected.map(s => if (s == label) "selected" else ""), href := "#/")(label)
  )
}

@JSExportAll
case class TodoItem(key: Key, completed: Boolean, label: String)

case class State(todos: List[TodoItem], toggleAll: Boolean)

enum Action {
  case Add(value: String)
  case SetCompletedAll(completed: Boolean)
  case ClearCompleted
  case Destroy(key: Key)
  case Toggle(key: Key)
  case ToggleAll
}

trait TodoItemObject extends js.Object {
  val completed: Boolean
  val label: String
}

import Action.*

@main def main() = {
  val ts                         = window.localStorage.getItem("todos")
  val INIT_TODOS: List[TodoItem] =
    if (ts == null) List.empty[TodoItem]
    else {
      JSON
        .parse(ts)
        .asInstanceOf[js.Array[TodoItemObject]]
        .map(o => TodoItem(createKey(), o.completed, o.label))
        .toList
    }
  val INIT_STATE                 = State(INIT_TODOS, false)

  val actions       = Subject[Action]()
  val enterPresseds = Subject[HTMLInputElement]()

  enterPresseds.subscribeNext(_.value = "")
  enterPresseds
    .map(_.value.trim())
    .filter(_ != "")
    .map(Action.Add.apply)
    .subscribe(actions)

  val state = actions
    .fold(INIT_STATE) { case (State(todos, toggleAll), action) =>
      val newTodos     = action match {
        case Add(v)                              => TodoItem(createKey(), false, v) :: todos
        case SetCompletedAll(completed: Boolean) => todos.map(_.copy(completed = completed))
        case ClearCompleted                      => todos.filterNot(_.completed)
        case Destroy(key)                        => todos.filterNot(_.key == key)
        case Toggle(key)                         =>
          todos.map {
            case TodoItem(k, completed, label) if k == key => TodoItem(k, !completed, label)
            case t                                         => t
          }
        case ToggleAll                           => todos.map(_.copy(completed = !toggleAll))
      }
      val newToggleAll = action match {
        case ToggleAll      => !toggleAll
        case ClearCompleted => false
        case _              => newTodos.nonEmpty && newTodos.forall(_.completed)
      }
      State(newTodos, newToggleAll)
    }
    .startWith(INIT_STATE)
    .remember()
  val todos = state.map(_.todos).remember()
  val empty = todos.map(_.isEmpty).remember()
  todos.subscribe(new Subscriber[List[TodoItem]] {
    override def onNext(t: List[TodoItem]): Unit = {
      import js.JSConverters.*
      val r = JSON.stringify(
        t.map(ti =>
          new TodoItemObject {
            override val completed: Boolean = ti.completed
            override val label: ByteString  = ti.label
          }
        ).toJSArray
      )
      window.localStorage.setItem("todos", r)
    }

    override def onCompleted: Unit = {}
  })

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
    empty.map(v =>
      if (v) EmptyNode
      else
        section(className := "main")(
          input(
            id        := "toggle-all",
            className := "toggle-all",
            `type`    := "checkbox",
            onchange  := actions.preProcess(_.mapTo(ToggleAll)),
            checked   := state.map(_.toggleAll)
          )(),
          label(`for` := "toggle-all")("Mark all as completed"),
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
                      input(
                        className := "toggle",
                        `type`    := "checkbox",
                        checked   := t.completed,
                        onchange  := actions.preProcess(_.mapTo(t.key).map(Toggle.apply))
                      )(),
                      label()(s"${t.label}"),
                      button(className := "destroy", onclick := actions.preProcess(_.mapTo(t.key).map(Destroy.apply)))("")
                    )
                  )
                ): _*
              )
            )
        )
    ),
    empty.map(v =>
      if (v) EmptyNode
      else
        footer(className := "footer")(
          span(className := "todo-count")(strong(todos.map(_.filterNot(_.completed).length).text()), " item left"),
          ul(className := "filters")(filters.map(_.view): _*),
          todos
            .map(_.exists(_.completed))
            .map(v =>
              if (v) button(className := "clear-completed", onclick := actions.preProcess(_.mapTo(ClearCompleted)))("Clear completed")
              else EmptyNode
            )
        )
    )
  )

  Renderer.render(document.body, app)
}
