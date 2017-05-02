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

}
