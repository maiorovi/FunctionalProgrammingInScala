package chapter4


sealed trait Either[+E, +A] {
  def map[B](f: A => B):Either[E, B] = this match {
    case Right(v) => Right(f(v))
    case Left(e) => Left(e)
  }

  def flatMap[EE >: E, B](f: A => Either[EE,B]):Either[EE,B] = this match {
    case Right(v) => f(v)
    case Left(v) => Left(v)
  }

  def orElse[EE >: E, B >: A](b: => Either[EE,B]):Either[EE,B] = this match {
    case Right(v) => Right(v)
    case Left(_) => b
  }

  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C):Either[EE, C] = this flatMap  (aa => b.map(bb => f(aa,bb)))

}
case class Left[+E](value: E) extends Either[E, Nothing]
case class Right[+A](value: A) extends Either[Nothing, A]


object Either {
  def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] = {
    def loop(es:List[Either[E,A]], acc:  Either[E, List[A]]):Either[E, List[A]] = es match {
      case x::xs => loop(xs, acc flatMap  (l => x map (v => v::l)))
      case Nil => acc
    }

    loop(es, Right(Nil))
  }


  def sequence1[E, A](es: List[Either[E, A]]): Either[E, List[A]] = traverse(es)(e => e)

  def traverse[E, A, B](as:List[A])(f: A => Either[E,B]): Either[E, List[B]] = as match {
    case h::t => f(h).map2(traverse(t)(f))((elem, list) => elem :: list)
    case Nil => Right(Nil)
  }
}

