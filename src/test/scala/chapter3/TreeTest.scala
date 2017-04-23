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

    assertResult(7) {
      Tree.sizeFold(tree)
    }
  }

  test("find maximum element in a tree") {
    assertResult(4) {
      Tree.maximum(tree)
    }

    assertResult(4) {
      Tree.maximumFold(tree)
    }
  }

  test("find the depth of a tree") {
    val treeDeep = Node(Leaf(5), Node(Node(Leaf(1), Leaf(2)), Node(Leaf(3), Leaf(4))))
    assertResult(2) {
      Tree.depth(tree)
    }

    assertResult(3) {
      Tree.depth(treeDeep)
    }
  }

  test("test maps a tree") {
    assertResult(Node(Node(Leaf(2), Leaf(4)), Node(Leaf(6), Leaf(8)))) {
      Tree.map(tree, (x: Int) => x * 2)
    }
  }

  test("sums a tree") {
    assertResult(10) {
      Tree.sum(tree)
    }
  }


}
