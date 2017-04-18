package chapter3

import scala.annotation.tailrec

trait MyList[+A]

case class Cons[A](head: A, tail: MyList[A]) extends MyList[A]

case object MyNil extends MyList[Nothing]

object MyList {
  def addOneToAll(xs: MyList[Int]): MyList[Int] = map(xs)( _ + 1 )

  def map[A, B](xs:MyList[A])(f: A => B):MyList[B] =  foldRightUsingMyFoldLeft(xs, MyNil:MyList[B])((x, acc) => Cons(f(x), acc))

  def doubleToString(xs:MyList[Double]):MyList[String] = foldRightUsingMyFoldLeft(xs, MyNil:MyList[String])((x, acc) => Cons(x.toString, acc) )

  def filter[A](as:MyList[A])(f: A => Boolean): MyList[A] =
    foldRightUsingMyFoldLeft(as, MyNil: MyList[A])((a, acc) => if (f(a)) Cons(a, acc) else acc)

  def filterViaFlatMap[A](as: MyList[A])(f: A => Boolean): MyList[A] =
    flatMap(as)( elem => if (f(elem)) Cons(elem, MyNil) else MyNil)

  def flatMap[A, B](xs:MyList[A])(f: A => MyList[B]):MyList[B] =
    foldRightUsingMyFoldLeft(xs, MyNil:MyList[B])((x, acc) => append(f(x), acc))

  //TODO: 3.15
  def foldRightUsingMyFoldLeft[A,B](xs:MyList[A], z:B)(f: (A, B) => B):B = myFoldLeft(reverse(xs), z)(f)

  def reverse[A](xs: MyList[A]): MyList[A] = {
    @tailrec
    def loop(ys:MyList[A], reversed:MyList[A]):MyList[A] = ys match {
      case Cons(z, zs) => loop(zs, Cons(z,reversed))
      case MyNil => reversed
    }

    loop(xs, MyNil)
  }

  def appendViaFoldLeft[A](xs:MyList[A], ys:MyList[A]):MyList[A] = myFoldRight(xs, ys)((x, acc) => Cons(x, acc))

  def reverseUsingFoldLeft[A,B](xs: MyList[A]):MyList[A] = myFoldLeft(xs, MyNil:MyList[A])((x, acc) => Cons(x, acc))

  def append[A](xs: MyList[A], ys: MyList[A]): MyList[A] = xs match {
    case MyNil => ys
    case Cons(z, zs) => Cons(z, append(zs, ys))
  }

  def dropWhile[A](list: MyList[A], p: A => Boolean): MyList[A] = {
    def loop(ys: MyList[A]): MyList[A] = ys match {
      case Cons(head, tail) => if (p(head)) loop(tail) else Cons(head, tail)
      case MyNil => MyNil
    }

    loop(list)
  }

  def setHead[A](xs: MyList[A], newHead: A): MyList[A] = xs match {
    case MyNil => throw new UnsupportedOperationException
    case Cons(head, tail) => Cons(newHead, tail)
  }

  def sum(xs: MyList[Int]): Int = myFoldLeft(xs, 0)(_ + _)

  def product(xs: MyList[Int]): Int = myFoldLeft(xs, 1)(_ * _)


  def myFoldRight[A, B](xs: MyList[A], z: B)(f: (A, B) => B): B = xs match {
    case Cons(y, ys) => f(y, myFoldRight(ys, z)(f))
    case MyNil => z
  }

  def myFoldLeft[A,B](xs: MyList[A], z:B)(f: (A,B) => B): B = {
    @tailrec
    def loop(ys:MyList[A], acc:B): B = ys match {
      case Cons(v, vs) =>  loop(vs, f(v, acc))
      case MyNil => acc
    }

    loop(xs, z)
  }

  def length[A](as: MyList[A]): Int = myFoldLeft(as, 0)((elem, acc) => acc + 1)

  def tail[A](xs: MyList[A]): MyList[A] = xs match {
    case MyNil => throw new UnsupportedOperationException
    case Cons(head, tail) => tail
  }

  def drop[A](xs: MyList[A], n: Int): MyList[A] = {
    @tailrec
    def loop(ys: MyList[A], n: Int): MyList[A] = (ys, n) match {
      case (ys, 0) => ys
      case (MyNil, n) => MyNil
      case (Cons(z, zs), n) => loop(zs, n - 1)
    }

    loop(xs, n)
  }

  def init[A](xs: MyList[A]): MyList[A] = xs match {
    case Cons(head, MyNil) => MyNil
    case Cons(y, ys) => Cons(y, init(ys))
  }
}
