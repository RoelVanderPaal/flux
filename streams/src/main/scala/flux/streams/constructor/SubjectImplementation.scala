package flux.streams.constructor

import flux.streams.{Subject, Subscriber}

case class SubjectImplementation[T]() extends AbstractObservable[T] with Subject[T] {
  override def onStart(): Unit = {}

  override def onNext(t: T): Unit = handleNext(t)

  override def onCompleted: Unit = handleCompleted()
}
