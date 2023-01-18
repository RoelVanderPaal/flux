package flux.streams

trait Subscriber[-T] {
  def onNext(t: T): Unit
  def onCompleted: Unit
}
