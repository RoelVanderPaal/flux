package flux.streams.constructor

import flux.streams.{Observable, Subscriber}

case class IterableObservable[T](i: Iterable[T]) extends AbstractObservable[T] {
  override def onStart(): Unit = {
    i.foreach(handleNext)
    handleCompleted()
  }
}
