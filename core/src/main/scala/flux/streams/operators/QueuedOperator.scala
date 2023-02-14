package flux.streams.operators

import flux.streams.{Observable, Subscriber}

import scala.collection.mutable.Queue
type Action          = () => Unit
type QueueSubscriber = Subscriber[Action]
case class QueuedOperator[T](o: Observable[T], queueSubscriber: QueueSubscriber, queue: Queue[Action] = QueuedOperator.queue)
    extends AbstractOperator[T, T](o) {
  override def handleOnNext(t: T): Unit = {
    val action: Action = () => handleNext(t)
    val wasEmpty       = queue.isEmpty
    queue += action
    if (wasEmpty) {
      queueSubscriber.onNext(action)
      queue.dequeue
      while (queue.nonEmpty) {
        val action = queue.dequeue
        queueSubscriber.onNext(action)
      }
    }
  }
}

object QueuedOperator {
  val queue = Queue[Action]()
}
