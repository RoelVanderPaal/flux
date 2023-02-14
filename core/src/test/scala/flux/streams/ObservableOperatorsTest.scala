package flux.streams

import flux.streams.operators.QueuedOperator
import flux.streams.util.{ListSubscriber, TestBase}

class ObservableOperatorsTest extends TestBase {
  test("map") {
    check(Observable.from(1, 2, 3).map(_ * 2), Seq(2, 4, 6))
  }
  test("filter") {
    check(Observable.from(1, 2, 3).filter(_ % 2 == 1), Seq(1, 3))
  }
  test("dropRepeats") {
    check(Observable.from(1, 1, 2, 2, 3).dropRepeats(), Seq(1, 2, 3))
  }

  test("drop") {
    check(Observable.from(1, 2, 3).drop(2), Seq(3))
  }
  test("fold") {
    check(Observable.from(1, 2, 3).fold(10)(_ + _), Seq(11, 13, 16))
  }
  test("merge") {
    check(Observable.merge(Observable.from(1, 2, 3), Observable.from(1, 2, 3).map(_ + 10)), Seq(1, 2, 3, 11, 12, 13))
  }
  test("queue") {
    val o      = Observable.from(1, 2, 3)
    val queue  = Subject[() => Unit]()
    val queued = QueuedOperator(o, queue)
    check(queued, Nil)
    queue.subscribeNext(_())
    check(queued, List(1, 2, 3))
  }
  test("queue2") {
    var l          = scala.collection.mutable.ArrayBuffer.empty[String]
    val o          = Subject[Int]()
    val queue      = Subject[() => Unit]()
    val queued     = QueuedOperator(o, queue)
    queue.subscribe(new Subscriber[() => Unit] {
      var added                                = false
      override def onNext(t: () => Unit): Unit = {
        l += "start"
        t()
        if (!added) {
          added = true
          o.onNext(4)
        }
        l += "end"
      }

      override def onCompleted: Unit = {}
    })
    val subscriber = ListSubscriber[Int]()
    queued.subscribe(subscriber)
    List(1, 2).foreach(o.onNext)
    subscriber.items shouldBe List(1, 4, 2)
    l.toSeq shouldBe Seq("start", "end", "start", "end", "start", "end")
  }

}
