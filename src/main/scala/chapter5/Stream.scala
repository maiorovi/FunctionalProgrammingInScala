package chapter5

import scala.annotation.tailrec

trait Stream[+A] {

  def headOption:Option[A] = this match {
    case Cons(h, t) => Some(h())
    case Empty => None
  }

  def take(i: Int):Stream[A] = this match {
    case Cons(h,t) if i == 0 => Stream.cons(h(), Stream.empty)
    case Cons(h,t) => Stream.cons(h(), t().take(i - 1))
    case _ => Empty
  }

  def map[B](f: A => B):Stream[B] = foldRight(Empty:Stream[B])((h, t) => Stream.cons(f(h), t))

  def filter(f: A => Boolean):Stream[A] = foldRight(Empty:Stream[A])((h,t) => if (f(h)) Stream.cons(h, t) else t )

  def drop(i:Int):Stream[A] = this match {
    case Cons(h, t) if i > 0 => t().drop(i-1)
    case _ => this
  }

  def headOptionViaFoldRight():Option[A] = foldRight(None: Option[A])((h, acc) => Some(h))

  def toList: List[A] = {
    @tailrec
    def loop(cur: Stream[A], acc: List[A]): List[A] = cur match {
      case Cons(h, t) => loop(t(), h() :: acc)
      case Empty => acc
    }

    loop(this, Nil).reverse
  }

  def takeWhile(p: A => Boolean):Stream[A] = this match {
    case Cons(h,t) if p(h()) => Stream.cons(h(), t().takeWhile(p))
    case Cons(_,_) => Empty
    case Empty => Empty
  }

  def foldRight[B](z:B)(f: (A, => B) => B):B = this match {
    case Cons(h,t) => f(h(), t().foldRight(z)(f))
    case Empty => z
  }

  def flatMap[B](f: A => Stream[B]):Stream[B] = foldRight(Empty:Stream[B])((h,t) => f(h) append t)

  def append[B >: A](s: Stream[B]):Stream[B] = foldRight(s)((h, t) => Stream.cons(h, t) )

  def exists(p: A => Boolean):Boolean = foldRight(false)((a, acc) => p(a) || acc)

  def forAll(p: A => Boolean):Boolean = foldRight(true)((a, acc) => p(a) && acc)

  def takeWhileViaFoldRight(p: A => Boolean):Stream[A] =
    foldRight(Empty:Stream[A])((a, acc) => if(p(a)) Stream.cons(a, acc) else Empty)
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