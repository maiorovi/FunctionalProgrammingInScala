package chapter5

import org.scalatest.{BeforeAndAfter, FunSuite}
import org.scalatest.Matchers._

class StreamTest extends FunSuite with BeforeAndAfter {

  var stream:Stream[Int] = _


  before {
    stream = Stream(1,2,3,4,5,6)
  }

  test("converts created stream to lis") {
    stream.toList should contain inOrder (1,2,3,4,5,6)
  }

  test("takes specified elements number from stream") {
    stream.take(3).toList should contain inOrder (1,2,3)
  }

  test("can take elemnts from stream while condition is true") {
    stream.takeWhile( _ < 5).toList should contain inOrder(1,2,3,4)
  }

  test("forall retuns true when specified condition is true for every element in stream") {
    stream.forAll( _ < 10) shouldBe true
  }

  test("forall retuns whether specified condition is true") {
    stream.forAll( _ < 3) shouldBe false
  }

  test("exist return true when at least one elem in stream satisfy given condition") {
    stream.exists( _ == 3 ) shouldBe true
  }

  test("exist return false when at any elem in stream don`t satisfy given condition") {
    stream.exists( _ == 53 ) shouldBe false
  }

  test("using map we can map over the stream") {
    stream.map(_ * 5).toList should contain inOrder (5,10,15,20,25,30)
  }

  test("using filter we can filter elements from the stream") {
    stream.filter(_ % 2 != 0).toList should contain inOrder (1,3,5)
  }

  test("append one stream to other") {
    val otherStream = Stream(7,8,9,10)
    stream.append(otherStream).toList should contain inOrder (1,2,3,4,5,6,7,8,9,10)
  }

}
