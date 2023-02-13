package flux.streams.operators

import flux.streams.{Observable, Subscriber}

type Queue = Subscriber[() => Unit]
case class QueuedOperator[T](o: Observable[T], queue: Queue) extends AbstractOperator[T, T](o) {
  override def handleOnNext(t: T): Unit = queue.onNext(() => handleNext(t))
}
