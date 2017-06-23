package chapter8

import chapter6.{RNG, State}

trait Prop {
  type SuccessCount = Int
  type FailedCase = String

  def check: Either[(FailedCase, SuccessCount), SuccessCount]

  def &&(p: Prop) = ???


}

case class Gen[A](sample: State[RNG, A])

object Gen {
  def choose(start:Int, stopExclusive:Int):Gen[Int] = Gen(State(RNG.nonNegativeInt).map( n => start + n % (stopExclusive - start)))

  def unit[A](a: => A): Gen[A] = Gen(State(RNG.unit(a)))

  def boolean:Gen[Boolean] = Gen(State(RNG.boolean))

//  def listOfN[A](n:Int, g: Gen[A]): Gen[List[A]] = Gen(State.sequence(List.fill(n)(g.sample)))
}