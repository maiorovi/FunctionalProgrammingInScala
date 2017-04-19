package chapter3

import org.scalatest.{BeforeAndAfter, FunSuite}

class TreeTest extends FunSuite with BeforeAndAfter {

  var tree : Tree[Int] = _

  before {
    tree = Node(Node(Leaf(1), Leaf(2)), Node(Leaf(3), Leaf(4)))
  }


  test("counts number of nodes in tree") {
      assertResult(7) {
      Tree.size(tree)
    }

    assertResult(7) {
      Tree.sizeTailRec(tree)
    }
  }


}
