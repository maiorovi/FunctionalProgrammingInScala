package chapter4

trait Option[+A] {
  def map[B](f: A => B): Option[B] = this match {
    case Some(a) => Some(f(a))
    case None => None
  }

  def flatMap[B](f: A => Option[B]): Option[B] = this match {
    case Some(x) => f(x)
    case None => None
  }

  def flatMapViaMap[B](f: A => Option[B]): Option[B] = map(x => f(x)).getOrElse(None)

  def getOrElse[B >: A](default: => B): B = this match {
    case Some(a) => a
    case None => default
  }

  def orElse[B >: A](ob: => Option[B]): Option[B] = this match {
    case Some(x) => Some(x)
    case None => ob
  }


  def orElseOther[B >: A](ob: => Option[B]): Option[B] = map(x => Some(x)).getOrElse(ob)

  def filter(f: A => Boolean): Option[A] = this match {
    case Some(a) if f(a) => Some(a)
    case None => None
  }

  def filterViaFlatMap(f: A => Boolean): Option[A] = flatMap(x => if (f(x)) Some(x) else None)
}

case class Some[+A](get: A) extends Option[A]

case object None extends Option[Nothing]


object Option {
  private def mean(xs: Seq[Double]): Option[Double] = if (xs.isEmpty) None else Some(xs.sum / xs.size)

  def variance(xs: Seq[Double]): Option[Double] = mean(xs).flatMap(m => mean(xs.map(x => Math.pow(x - m, 2))))

  def lift[A, B](f: A => B): Option[A] => Option[B] = _ map (v => f(v))

  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = (a, b) match {
    case (Some(a), Some(b)) => Some(f(a, b))
    case (_, _) => None
  }

  def map2ViaFlatMap[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = a flatMap (a1 => b.map(b1 => f(a1, b1)))

  def sequence[A](a: List[Option[A]]): Option[List[A]] = a match {
    case x :: xs => x.flatMap(xx => sequence(xs) map (tt => xx :: tt))
    case Nil => Some(Nil)
  }

  //straitforward way
  //  def traverse[A,B](a: List[A])(f: A => Option[B]): Option[List[B]] = sequence(a.map(f))
  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = a match {
    case  x::xs => f(x).flatMap( xx => traverse(xs)(f).map(yy => (xx::yy)))
    case Nil => Some(Nil)
  }

  def sequenceViaTraverse[A](a: List[Option[A]]): Option[List[A]] = traverse(a)(x => x)

}
