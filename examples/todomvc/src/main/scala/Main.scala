import flux.streams.constructor.EventListenerObservable
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

case class State(todos: List[TodoItem], toggleAll: Boolean, edit: Option[Key] = None)

enum Action {
  case Add(value: String)
  case SetCompletedAll(completed: Boolean)
  case ClearCompleted
  case Destroy(key: Key)
  case Toggle(key: Key)
  case ToggleAll
  case Edit(key: Key)
  case Save(key: Key, value: String)
  case ExitEdit
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
  val edit          = Subject[Option[Key]]()

  enterPresseds.subscribeNext(_.value = "")
  enterPresseds
    .map(_.value.trim())
    .filter(_ != "")
    .map(Action.Add.apply)
    .subscribe(actions)
  EventListenerObservable[KeyboardEvent](document, "keyup").filter(_.key == "Escape").mapTo(ExitEdit).subscribe(actions)

  val state = actions
    .fold(INIT_STATE) { case (State(todos, toggleAll, e), action) =>
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
        case Save(key, label)                    =>
          todos.map {
            case TodoItem(k, completed, _) if k == key => TodoItem(k, completed, label)
            case t                                     => t
          }
        case _                                   => todos
      }
      val newToggleAll = action match {
        case ToggleAll      => !toggleAll
        case ClearCompleted => false
        case _              => newTodos.nonEmpty && newTodos.forall(_.completed)
      }
      val newEdit      = action match {
        case Edit(ne)   => Some(ne)
        case ExitEdit   => None
        case Save(_, _) => None
        case _          => e
      }
      State(newTodos, newToggleAll, newEdit)
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
          _.filter(_.key == "Enter")
            .map(_.target.asInstanceOf[HTMLInputElement])
        )
      )()
    ),
    empty
//      .dropRepeats()
      .map(v =>
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
              .combine(state, selectedFilter)
              .map {
                case (state, f) => {
                  val ts  = state.todos
                  val tds = f match {
                    case "All"       => ts
                    case "Active"    => ts.filterNot(_.completed)
                    case "Completed" => ts.filter(_.completed)
                  }
                  ul(
                    className := "todo-list"
                  )(
                    tds.map(t =>
                      li(
                        className := List(
                          Option.when(t.completed)("completed"),
                          state.edit.flatMap(k => Option.when(t.key == k)("editing"))
                        ).flatten.mkString(" ")
                      )(
                        div(className := "view")(
                          input(
                            className := "toggle",
                            `type`    := "checkbox",
                            checked   := t.completed,
                            onchange  := actions.preProcess(_.mapTo(t.key).map(Toggle.apply))
                          )(),
                          label(ondblclick := actions.preProcess(_.mapTo(Edit(t.key))))(s"${t.label}"),
                          button(className := "destroy", onclick := actions.preProcess(_.mapTo(t.key).map(Destroy.apply)))("")
                        ),
                        input(
                          className := "edit",
                          value     := t.label,
                          onkeyup   := actions.preProcess(
                            _.filter(_.key == "Enter")
                              .map(_.target.asInstanceOf[HTMLInputElement])
                              .map(e => {
                                val trim = e.value.trim
                                if (trim == "") Destroy(t.key) else Save(t.key, trim)
                              })
                          ),
                          onblur    := actions.preProcess(
                              .map(_.target.asInstanceOf[HTMLInputElement])
                              .map(e => {
                                val trim = e.value.trim
                                if (trim == "") Destroy(t.key) else Save(t.key, trim)
                              })
                          )
                        )()
                      )
                    ): _*
                  )
                }
              }
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
