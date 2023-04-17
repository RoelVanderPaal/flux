package flux.streams.operators

import flux.streams.{Observable, Subscriber}

case class TapOperator[T](o: Observable[T], f: T => Unit) extends AbstractOperator[T, T](o) {
  override def handleOnNext(t: T): Unit = {
    f(t)
    handleNext(t)
  }
}
