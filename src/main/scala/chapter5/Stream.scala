package chapter5

import scala.annotation.tailrec

trait Stream[+A] {
  def take(i: Int):Stream[A] = this match {
    case Cons(h,t) if i == 0 => Stream.cons(h(), Stream.empty)
    case Cons(h,t) => Stream.cons(h(), t().take(i - 1))
    case _ => Empty
  }

  def drop(i:Int):Stream[A] = this match {
    case Cons(h, t) if i > 0 => t().drop(i-1)
    case _ => this
  }

  def headOption: Option[A] = this match {
    case Cons(h, t) => Some(h())
    case Empty => None
  }


  def toList: List[A] = {
    @tailrec
    def loop(cur: Stream[A], acc: List[A]): List[A] = cur match {
      case Cons(h, t) => loop(t(), h() :: acc)
      case Empty => acc
    }

    loop(this, Nil).reverse
  }
}

case class Cons[+A](h: () => A, t:() => Stream[A]) extends Stream[A]
case object Empty extends Stream[Nothing]

object Stream {

  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl

    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](xs: A*): Stream[A] = {
    if (xs.isEmpty) empty else cons(xs.head, apply(xs.tail: _*))
  }

}