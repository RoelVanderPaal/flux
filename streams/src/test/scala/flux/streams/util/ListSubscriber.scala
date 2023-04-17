package flux.streams.util

import flux.streams.Subscriber

class ListSubscriber[T]() extends Subscriber[T] {
  private var is                  = List.empty[T]
  var completed                   = false
  override def onNext(t: T): Unit = is ::= t

  override def onCompleted: Unit = completed = true

  def items: Seq[T] = is.reverse
}
