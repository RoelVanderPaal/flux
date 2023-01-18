package flux.streams.constructor

import flux.streams.{Subject, Subscriber}

case class SubjectImplementation[T]() extends AbstractObservable[T] with Subject[T]:
  override def onStart(): Unit = {}

  override def onNext(t: T): Unit = subscribers.foreach(_.onNext(t))

  override def onCompleted: Unit = subscribers.foreach(_.onCompleted)
