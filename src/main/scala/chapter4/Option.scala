package chapter4

trait Option[+A] {
  def map[B](f: A => B):Option[B] = this match {
    case Some(a) => Some(f(a))
    case None => None
  }

  def flatMap[B](f: A => Option[B]):Option[B] = this match {
    case Some(x) => f(x)
    case None => None
  }

  def flatMapViaMap[B](f: A => Option[B]):Option[B] = map(x => f(x)).getOrElse(None)

  def getOrElse[B >: A](default: => B):B = this match {
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

  def filterViaFlatMap(f: A => Boolean): Option[A] = flatMap(x => if(f(x)) Some(x) else None)
}
case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]


object Option {

}
