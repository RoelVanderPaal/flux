package flux.streams

import flux.streams.util.{ListSubscriber, TestBase}

class ObservableTest extends TestBase {
  test("An Observable should only start when it has been subscribed to and stop when there are no subscriptions") {
    val subscriber1 = new ListSubscriber[Int]()
    val subscriber2 = new ListSubscriber[Int]()

    val subject = Subject[Int]()
    subject.onNext(0)
    subject.subscribe(subscriber1)
    subject.onNext(1)
    subject.subscribe(subscriber2)
    subject.onNext(2)
    subject.unsubscribe(subscriber1)
    subject.onNext(3)
    subject.unsubscribe(subscriber2)
    subject.onNext(4)

    subscriber1.items shouldBe Seq(1, 2)
    subscriber2.items shouldBe Seq(2, 3)
  }
}
