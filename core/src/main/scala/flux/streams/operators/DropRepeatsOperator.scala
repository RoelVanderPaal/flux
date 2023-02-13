package flux.streams.operators

import flux.streams.{Observable, Subscriber}

case class DropRepeatsOperator[T](o: Observable[T], isEqual: (T, T) => Boolean) extends AbstractOperator[T, T](o) {
  var last                                                                    = Option.empty[T]
  override def handleOnNext(t: T): Unit = {
    if (!last.exists(isEqual(_, t))) {
      handleNext(t)
    }
    last = Some(t)
  }

}
