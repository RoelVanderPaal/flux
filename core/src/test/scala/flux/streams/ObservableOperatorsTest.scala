package flux.streams

import flux.streams.util.{ListSubscriber, TestBase}

class ObservableOperatorsTest extends TestBase {
  test("map") {
    check(Observable.from(1, 2, 3).map(_ * 2), Seq(2, 4, 6))
  }
  test("filter") {
    check(Observable.from(1, 2, 3).filter(_ % 2 == 1), Seq(1, 3))
  }
  test("drop") {
    check(Observable.from(1, 2, 3).drop(2), Seq(3))
  }
  test("fold") {
    check(Observable.from(1, 2, 3).fold(10)(_ + _), Seq(11, 13, 16))
  }
}
