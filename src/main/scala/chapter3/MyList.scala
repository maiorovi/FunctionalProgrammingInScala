package chapter3

import scala.annotation.tailrec

trait MyList[+A]
case class Cons[A](head:A, tail: MyList[A]) extends MyList[A]
case object MyNil extends MyList[Nothing]

object MyList {
  def append[A](xs: MyList[A], ys: MyList[A]): MyList[A] = xs match {
    case MyNil => ys
    case Cons(z, zs) => Cons(z, append(zs, ys))
  }

  def dropWhile[A](list: MyList[A], p: A => Boolean): MyList[A] = {
    def loop(ys:MyList[A]):MyList[A] = ys match {
      case Cons(head, tail) => if (p(head)) loop(tail) else Cons(head, tail)
      case MyNil => MyNil
    }

    loop(list)
  }

  def setHead[A](xs: MyList[A], newHead: A): MyList[A] = xs match {
    case MyNil => throw new UnsupportedOperationException
    case Cons(head, tail) => Cons(newHead, tail)
  }

  def sum(xs:MyList[Int]):Int = xs  match {
    case MyNil => 0
    case Cons(head, tail) => head + sum(tail)
  }

  def product(xs:MyList[Int]):Int = xs match {
    case MyNil =>1
    case Cons(0, tail) => 0
    case Cons(head,tail) => head * product(tail)
  }

  def tail[A](xs:MyList[A]):MyList[A] = xs match {
    case MyNil => throw new UnsupportedOperationException
    case Cons(head, tail) => tail
  }

  def drop[A](xs:MyList[A], n:Int):MyList[A] = {
    @tailrec
    def loop(ys:MyList[A], n:Int):MyList[A] = (ys, n) match {
      case (ys,0) => ys
      case (MyNil, n) => MyNil
      case (Cons(z, zs), n) => loop(zs, n-1)
    }

    loop(xs, n)
  }

  def init[A](xs:MyList[A]):MyList[A] = xs match {
    case Cons(head, MyNil) => MyNil
    case Cons(y, ys) => Cons(y, init(ys))
  }
}
