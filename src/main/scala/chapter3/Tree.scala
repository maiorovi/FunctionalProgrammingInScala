package chapter3

import scala.annotation.tailrec

sealed trait Tree[+A]
case class Leaf[A](value:A) extends Tree[A]
case class Node[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {
  def size[A](tree: Tree[A]): Int = tree match {
    case Leaf(value) => 1
    case Node(l, r) => 1 + size(l) + size(r)
  }

  def sizeTailRec[A](tree:Tree[A]):Int = {
    @tailrec
    def loop(trees:List[Tree[A]], acc:Int):Int = trees match {
      case Nil => acc
      case Leaf(value)::rs => loop(rs, acc + 1)
      case Node(l, r)::rs => loop(l::r::rs, acc+1)
    }

    loop(List(tree), 0)
  }

}
