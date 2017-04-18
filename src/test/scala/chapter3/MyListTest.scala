package chapter3

import org.scalatest.{BeforeAndAfter, FunSuite}

class MyListTest extends FunSuite with BeforeAndAfter {

  var list: MyList[Int] = _

  before {
    list = Cons(1, Cons(2, Cons(3, MyNil)))
  }

  test("adds all list member") {
    assertResult(6) {
      MyList.sum(list)
    }
  }

  test("counts product of all list members") {
    list = Cons(5, list)

    assertResult(30) {
      MyList.product(list)
    }
  }

  test("throws unsupported operation exception when doing tail on empty list") {
    assertThrows[UnsupportedOperationException] {
      MyList.tail(MyNil)
    }
  }

  test("removes first elements from the list") {
    val expectedList = Cons(2, Cons(3, MyNil))

    assertResult(expectedList) {
      MyList.tail(list)
    }
  }

  test("setsHead of a list to a given value") {
    val expectedList = Cons(12, Cons(2, Cons(3, MyNil)))

    assertResult(expectedList) {
      MyList.setHead(list, 12)
    }
  }

  test("drops given number element from list") {
    assertResult(Cons(3, MyNil)) {
      MyList.drop(list, 2)
    }
  }

  test("returns MyNil on drop call if N higher then list length") {
    assertResult(MyNil) {
      MyList.drop(list, 5)
    }
  }

  test("drops while condition is met") {
    assertResult(Cons(3, MyNil)) {
      MyList.dropWhile(list, (a:Int) => a != 3)
    }
  }

  test("appends two lists") {
    val secondList = Cons(4, Cons(5, Cons(6, MyNil)))

    assertResult(Cons(1,Cons(2, Cons(3,Cons(4, Cons(5, Cons(6, MyNil))))))) {
      MyList.append(list, secondList)
    }
  }

  test("removes last element in a list") {
    assertResult(Cons(1, Cons(2, MyNil))) {
      MyList.init(list)
    }
  }

  test("computes length of a list") {
    assertResult(3) {
      MyList.length(list)
    }
  }

  test("reverses list") {
    assertResult(Cons(3, Cons(2, Cons(1, MyNil)))) {
      MyList.reverse(list)
    }
  }

  test("reverses list using fold left") {
    assertResult(Cons(3, Cons(2, Cons(1, MyNil)))) {
      MyList.reverseUsingFoldLeft(list)
    }
  }

  test("appends first list to second list") {
    assertResult(Cons(1, Cons(2, Cons(3, Cons(4, Cons(5, MyNil)))))) {
      MyList.append(list, Cons(4, Cons(5, MyNil)))
    }
  }

  test("adds one to all elements of a list") {
    assertResult((Cons(2, Cons(3, Cons(4, MyNil))))) {
      MyList.addOneToAll(list)
    }
  }

  test("convertes doubles list to strings list") {
    assertResult(Cons("1.0", Cons("2.0", Cons("3.0", MyNil)))) {
      MyList.doubleToString(Cons(1d, Cons(2d, Cons(3d, MyNil))))
    }
  }

  test("flat maps list") {
    assertResult(Cons(1, Cons(1, Cons(2, Cons(2, Cons(3, Cons(3, MyNil))))))) {
      MyList.flatMap(list)(elem => Cons(elem, Cons(elem, MyNil)))
    }
  }

}
