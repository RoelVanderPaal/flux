package flux.streams

import flux.streams.constructor.SubjectImplementation

trait Subject[T] extends Observable[T] with Subscriber[T]

object Subject {
  def apply[T](): Subject[T] = SubjectImplementation[T]()
}
