package chapter3

import scala.annotation.tailrec

sealed trait Tree[+A]
case class Leaf[A](value:A) extends Tree[A]
case class Node[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {
  def map[A, B](tree: Tree[A], f: A => B): Tree[B] = tree match {
    case Leaf(value) => Leaf(f(value))
    case Node(left,right) => Node(map(left, f), map(right, f))
  }


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

  def maximum(tree:Tree[Int]):Int = tree match {
    case Leaf(value) => value
    case Node(left, right) => Math.max(maximum(left), maximum(right))
  }

  def maximumTailRec(tree:Tree[Int]):Int = {
    @tailrec
    def loop(trees:List[Tree[Int]], max:Int):Int = trees match {
      case Nil => max
      case Leaf(value)::rs => loop(rs, Math.max(value, max))
      case Node(left, right)::rs => loop(left::right::rs, max)
    }

    loop(List(tree), Integer.MIN_VALUE)
  }

  def depth(tree:Tree[Int]):Int = tree match {
    case Leaf(value) => 0
    case Node(left,right) => Math.max(depth(left)+1, depth(right)+1)
  }


}
