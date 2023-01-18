package flux.streams

import flux.streams.util.{ListSubscriber, TestBase}

class ObservableVarianceTest extends TestBase {
  test("variance") {
    class Animal
    class Cat     extends Animal
    class Dog     extends Animal
    class TinyDog extends Dog

    val tinyDog = new TinyDog()
    val dog     = new Dog()
    val cat     = new Cat()
    val animal  = new Animal()

    val tinyDogObservable: Observable[TinyDog] = Observable.from(tinyDog)
    val dogObservable: Observable[Dog]         = Observable.from(dog)
    val catObservable: Observable[Cat]         = Observable.from(cat)
    val animalObservable: Observable[Animal]   = Observable.from(animal)

    val tinyDogSubscriber = new ListSubscriber[TinyDog]()
    val dogSubscriber     = new ListSubscriber[Dog]()
    val catSubscriber     = new ListSubscriber[Cat]()
    val animalSubscriber  = new ListSubscriber[Animal]()

    val test: Observable[Animal] = tinyDogObservable
    tinyDogObservable.subscribe(tinyDogSubscriber)
    tinyDogObservable.subscribe(dogSubscriber)
    tinyDogObservable.subscribe(animalSubscriber)
  }
}
