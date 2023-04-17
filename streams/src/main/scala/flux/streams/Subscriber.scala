package flux.streams

import flux.streams.constructor.ProcessingSubjectImplementation

trait Subscriber[-T] {
  def onNext(t: T): Unit
  def onCompleted: Unit

  def preProcess[U](f: Observable[U] => Observable[T]): Subscriber[U] = {
    val subject    = Subject[U]()
    val observable = f(subject)
    observable.subscribe(this)
    ProcessingSubjectImplementation(subject, observable)
  }
}
