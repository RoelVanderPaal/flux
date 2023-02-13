package flux.streams.operators

import flux.streams.{Observable, Subscriber}

case class FoldOperator[T, U](o: Observable[T], z: U, f: (U, T) => U) extends AbstractOperator[T, U](o) {
  private var accumulator                                                     = z
  override def handleOnNext(t: T): Unit = {
    accumulator = f(accumulator, t)
    handleNext(accumulator)
  }
}
