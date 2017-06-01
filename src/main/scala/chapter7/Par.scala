package chapter7
import java.util.concurrent.Callable
import java.util.concurrent.{ExecutorService, Future, TimeUnit}

object Par {
  type Par[A] = ExecutorService => Future[A]



  private case class UnitFuture[A](a: A) extends Future[A] {
    override def cancel(mayInterruptIfRunning: Boolean): Boolean = false

    override def isCancelled: Boolean = false

    override def isDone: Boolean = true

    override def get(): A = a

    override def get(timeout: Long, unit: TimeUnit): A = get
  }

  def run[A](s: ExecutorService)(a: Par[A]): A = ???

  def unit[A](a: A):Par[A] = es => UnitFuture(a)

  def map2[A,B,C](a:Par[A], b:Par[B])(f: (A,B) => C):Par[C] = es => {
    val a1 = a(es)
    val b1 = b(es)

    UnitFuture(f(a1.get, b1.get))
  }

  def map[A,B](a:Par[A])(f: A => B):Par[B] = map2(a, unit(()))((x, _) => f(x))

  def fork[A](a: => Par[A]):Par[A] = es => {
    es.submit(new Callable[A] {
      def call = a(es).get
    })
  }

  def lazyUnit[A](a: => A):Par[A] = fork(unit(a))

  def asyncF[A,B](f: A => B):A => Par[B] = a => lazyUnit(f(a))

  def sortPar(parList: Par[List[Int]]): Par[List[Int]] = map(parList)(l => l.sortWith(_ > _))

  def parMap[A,B](ps: List[A])(f: A => B): Par[List[B]] = fork {
    val fbs: List[Par[B]] = ps.map(asyncF(f))

    sequence(fbs)
  }

  def sequence[A](ps: List[Par[A]]): Par[List[A]] = ps.foldLeft(unit(List[A]()))((acc, p) => map2(acc, p)((acc,p) => p::acc))


//  def parFilter[A](ps: List[A])(f: Par[A] => Boolean):Par[List[A]] =
//    ps.foldLeft(unit(List[A]()))((acc, p) => map(acc)((acc) => p::acc))

  def parFilter[A](ps: List[A])(f: A => Boolean):Par[List[A]] = {
    // List[A] to List[Par[A]]
    val fbs = ps.map(unit(_))

    filteringSequence(fbs)(f)
  }

  private def filteringSequence[A](ps: List[Par[A]])(f: A => Boolean): Par[List[A]] = ps.foldLeft(unit(List[A]()))((acc, p) => map2(acc, p)((acc,p) => if (f(p)) p::acc else acc))



}
