package chapter6

import org.scalatest.{BeforeAndAfter, FunSuite}
import org.scalatest.Matchers._
import org.scalamock.scalatest.MockFactory
import org.scalatest.FlatSpec

class RNGTest extends FunSuite with BeforeAndAfter with MockFactory {

  var rng:RNG = _

  before {
    rng = SimpleRNG(10)
  }

  test("generator generates random int number but also creates new generator with updeted state") {
    rng.nextInt shouldBe (240449,SimpleRNG(15758065741L))
  }

  test("generates random pair of integers") {
    RNG.randomPair(rng) shouldBe ((240449,-898330386),SimpleRNG(222601996553940L))
  }

  test("generates double greater then zero less then one") {
    val mockedRng = stub[RNG]

    (mockedRng.nextInt _).when().returns((350000, mockedRng))

    RNG.double(mockedRng) shouldBe ((1.629814506336029E-4,  mockedRng))
  }

  test("generates random int, double pair") {
    val mockedRng = stub[RNG]

    (mockedRng.nextInt _).when().returns((3, mockedRng))

    RNG.intDouble(mockedRng) shouldBe((3, 1.396983862573739E-9), mockedRng)
  }

  test("generates list of random integers") {
    val mockedRng = stub[RNG]

    (mockedRng.nextInt _).when().returns((5, mockedRng))

    RNG.ints(5)(mockedRng)._1 shouldBe List(5,5,5,5,5)
  }

  test("simulate machine test") {
    val machine = Machine(true, 15, 0)

    val actions = List(Coin, Turn)

    State.simulateMachine(actions)(machine) shouldBe ((Machine(true, 14, 1), (1, 14)))
  }


}
