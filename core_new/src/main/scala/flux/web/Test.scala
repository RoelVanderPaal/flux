package flux.web

import flux.streams.Subject
import org.scalajs.dom.*

object Test extends App {
  val h = href     := "test"
  val c = checked  := true
  val d = disabled := true

  val s_e              = Subject[Element]()
  val s_h              = Subject[HTMLElement]()
  val s_d              = Subject[HTMLDivElement]()
  val s_i              = Subject[HTMLInputElement]()
  val s_b              = Subject[HTMLButtonElement]()
  val s_string         = Subject[String]()
  val s_event          = Subject[Event]()
  val s_keyboard_event = Subject[KeyboardEvent]()

  button(h, d, ref := ((_: HTMLButtonElement).focus()))()
  div(h)()
  div(href := s_string, onchange := s_event, onkeydown := s_keyboard_event)(
    input(c, h, h, d)(),
    input(c, h, h, d)()
  )
}
