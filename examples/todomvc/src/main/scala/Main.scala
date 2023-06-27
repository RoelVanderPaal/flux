import flux.*
import flux.streams.constructor.EventListenerObservable
import flux.streams.{Observable, RememberSubject, Subject, Subscriber}
import org.scalajs.dom.*

import java.util.UUID
import scala.scalajs.js
import scala.scalajs.js.JSON
import scala.scalajs.js.annotation.JSExportAll
import scala.scalajs.js.timers.*
import scala.util.Random

type Key = String
def createKey(): Key = Random.nextDouble().toString
case class Filter(selected: Observable[String])(title: String) {
  private val clicks = Subject[MouseEvent]()
  val output         = clicks.mapTo(title)
  val view           = li()(
    a(onclick := clicks, className := selected.map(s => if (s == title) "selected" else ""), href := "#/")(title)
  )
}

@JSExportAll
case class TodoItem(key: Key, completed: Boolean, title: String)

case class State(todos: List[TodoItem], toggleAll: Boolean, edit: Option[Key] = None)

enum Action {
  case Add(value: String)
  case SetCompletedAll(completed: Boolean)
  case ClearCompleted
  case Destroy
  case Toggle(key: Key)
  case ToggleAll
  case Edit(key: Key)
  case Save(value: String)
  case ExitEdit
}

trait TodoItemObject extends js.Object {
  val completed: Boolean
  val title: String
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
        .map(o => TodoItem(createKey(), o.completed, o.title))
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
      println(action)
      val newTodos     = action match {
        case Add(v)                              => todos :+ TodoItem(createKey(), false, v)
        case SetCompletedAll(completed: Boolean) => todos.map(_.copy(completed = completed))
        case ClearCompleted                      => todos.filterNot(_.completed)
        case Destroy                             => e.map(key => todos.filterNot(_.key == key)).getOrElse(todos)
        case Toggle(key)                         =>
          todos.map {
            case TodoItem(k, completed, title) if k == key => TodoItem(k, !completed, title)
            case t                                         => t
          }
        case ToggleAll                           => todos.map(_.copy(completed = !toggleAll))
        case Save(title)                         =>
          e.map(key =>
            todos.map {
              case TodoItem(k, completed, _) if k == key => TodoItem(k, completed, title)
              case t                                     => t
            }
          ).getOrElse(todos)
        case _                                   => todos
      }
      val newToggleAll = action match {
        case ToggleAll      => !toggleAll
        case ClearCompleted => false
        case _              => newTodos.nonEmpty && newTodos.forall(_.completed)
      }
      val newEdit      = action match {
        case Edit(ne) => Some(ne)
        case ExitEdit => None
        case Save(_)  => None
        case _        => e
      }
      State(newTodos, newToggleAll, newEdit)
    }
    .tap(println)
    .startWith(INIT_STATE)
    .remember()
  val todos = state.map(_.todos).dropRepeats().remember()
  val empty = todos.map(_.isEmpty).dropRepeats().remember()
  todos.subscribe(new Subscriber[List[TodoItem]] {
    override def onNext(t: List[TodoItem]): Unit = {
      import js.JSConverters.*
      val r = JSON.stringify(
        t.map(ti =>
          new TodoItemObject {
            override val completed: Boolean = ti.completed
            override val title: ByteString  = ti.title
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
  val newInputRef    = Subject[HTMLElement]()
  newInputRef.subscribeNext(_.focus())

  val app = section(className := "todoapp")(
    header(className := "header")(
      h1("todos"),
      input(
        className   := "new-todo",
        placeholder := "What needs to be done?",
        autofocus   := true,
        ref         := newInputRef,
        onkeyup     := enterPresseds.preProcess(
          _.filter(_.key == "Enter")
            .map(_.target.asInstanceOf[HTMLInputElement])
        )
      )()
    ),
    empty
      .map(v =>
        if (v) EmptyNode
        else
          section(className := "main")(
            input(
              id        := "toggle-all",
              checked   := state.map(_.toggleAll),
              className := "toggle-all",
              `type`    := "checkbox",
              onchange  := actions.preProcess(_.mapTo(ToggleAll))
            )(),
            label(`for` := "toggle-all")("Mark all as complete"),
            Observable
              .combine(state, selectedFilter)
              .map { case (s, f) =>
                val ts = s.todos
                f match {
                  case "All"       => ts
                  case "Active"    => ts.filterNot(_.completed)
                  case "Completed" => ts.filter(_.completed)
                }
              }
              .dropRepeats()
              .map(todos => {

                ul(
                  className := "todo-list"
                )(
                  todos.map(t => {
                    val inputRef        = Subject[HTMLElement]()
                    val componentUpdate = Subject[HTMLElement]()
                    inputRef
//                      .combine(componentUpdate)
                      .combine(state.map(_.edit).filter(_.exists(_ == t.key)))
                      .subscribeNext(_._1.focus())
                    li(
                      data      := Map("testid" -> "todo-item"),
                      className := state
                        .map(_.edit)
                        .map(edit =>
                          List(
                            Option.when(t.completed)("completed"),
                            edit.map(x => if (x == t.key) "editing" else "")
                          ).flatten.mkString(" ")
                        )
                    )(
                      div(className := "view")(
                        input(
                          id        := t.title,
                          className := "toggle",
                          `type`    := "checkbox",
                          checked   := t.completed,
                          onchange  := actions.preProcess(_.mapTo(t.key).map(Toggle.apply))
                        )(),
                        label(data := Map("testid" -> "todo-title"), ondblclick := actions.preProcess(_.mapTo(t.key).map(Edit.apply)))(
                          t.title
                        ),
                        button(className := "destroy", onclick := actions.preProcess(_.mapTo(t.key).map(_ => Destroy)))("")
                      ),
                      input(
                        ref       := inputRef,
                        ariaLabel := "Edit",
                        className := "edit",
                        value     := t.title,
                        onkeyup   := actions.preProcess(
                          _.filter(_.key == "Enter")
                            .map(_.target.asInstanceOf[HTMLInputElement])
                            .map(e => {
                              val trim = e.value.trim
                              if (trim == "") Destroy else Save(trim)
                            })
                        ),
                        onblur    := actions.preProcess(
                          _.map(_.target.asInstanceOf[HTMLInputElement])
                            .map(e => {
                              val trim = e.value.trim
                              if (trim == "") Destroy else Save(trim)
                            })
                        )
                      )()
                    )
                  }): _*
                )
              })
          )
      ),
    empty.map(v =>
      if (v) EmptyNode
      else
        footer(className := "footer")(
          div(className := "todo-count", data := Map("testid" -> "todo-count"))(
            todos
              .map(_.filterNot(_.completed).length)
              .map(c => List(strong(c.toString), span(" "), if (c > 1) span("items") else span("item"), span(" left")))
          ),
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
