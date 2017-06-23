package chapter6

import chapter6.RNG.Rand

import scala.collection.mutable


trait RNG {

  def nextInt: (Int, RNG)

}

case class SimpleRNG(seed: Long) extends RNG {
  override def nextInt: (Int, RNG) = {
    val newSeed = (seed * 0x5DECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val newRng = SimpleRNG(newSeed)
    val n = (newSeed >>> 16).toInt
    (n, newRng)
  }
}


object RNG {

  def int:Rand[Int] = _.nextInt

  def double(rng: RNG): (Double, RNG) = {
    val (i1, newRng) = rng.nextInt

    (i1.toDouble / Integer.MAX_VALUE.toDouble ,newRng)
  }

  def intDouble(rng:RNG): ((Int, Double), RNG) = {
    val (i1, rng1) = rng.nextInt
    val (d1, rng2) = double(rng1)

    ((i1, d1), rng2)
  }

  def doubleInt(rng:RNG): ((Double, Int), RNG) = {
    val ((i1, d1), rng2) = intDouble(rng)
    ((d1, i1), rng2)
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (d1, rng1) = double(rng)
    val (d2, rng2) = double(rng1)
    val (d3, rng3) = double(rng2)

    ((d1, d2, d3), rng3)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    var i = 0
    var currentRng = rng
    val xs = mutable.MutableList[Int]()

    while (i < count) {
      val (i1, tmpRng) = rng.nextInt
      xs += i1
      currentRng = tmpRng
      i += 1
    }

    (xs.toList, currentRng)
  }

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (i1, rng2) = rng.nextInt

    if (i1 == Integer.MIN_VALUE) {
      (0 ,rng2)
    } else {
      (Math.abs(i1), rng2)
    }
  }

  def boolean(rng:RNG):(Boolean, RNG) = {
    val (newInt, newRng) = rng.nextInt

    if (newInt % 2 == 0) (true, newRng) else (false, newRng)
  }

  type Rand[+A] = RNG => (A, RNG)

  def nonNegativeEven: Rand[Int] = map(nonNegativeInt)(i => i - i % 2)

  def unit[A](a: A): Rand[A] = rng => (a, rng)

  def map[A,B](s:Rand[A])(f:A => B): Rand[B] = rng => {
    val (a, rng2) = s(rng)
    (f(a), rng2)
  }

  def doubleWithMap:Rand[Double] = map(_.nextInt)(i => i.toDouble / Integer.MAX_VALUE.toDouble)

  def map2[A,B,C](ra:Rand[A], rb: Rand[B])(f: (A,B) => C):Rand[C] = rng => {
    val (a, rngA) = ra(rng)
    val (b, rngB) = rb(rngA)

    (f(a,b), rngB)
  }

  def both[A,B](ra:Rand[A], rb: Rand[B]):Rand[(A,B)] = map2(ra,rb)((_, _))

  def randIntDouble: Rand[(Double,Int)] = both(double, int)
  def randDoubleInt: Rand[(Int, Double)] = both(int, double(_))


  def sequence[A](xs:List[Rand[A]]):Rand[List[A]] = xs.foldRight(unit(List[A]()))((x, acc) => map2(x, acc)(_ :: _))

  def intsViaSequence(count: Int)(rng: RNG): Rand[List[Int]] = sequence(List.fill(count)(int))

  def randomPair(rng: RNG) : ((Int, Int), RNG) = {
    val (i1, rng2) = rng.nextInt
    val (i2, rng3) = rng2.nextInt

    ((i1, i2), rng3)
  }

  def flatMap[A,B](ra:Rand[A])(g: A => Rand[B]):Rand[B] = rng => {
    val (a, rng1) = ra(rng)
    g(a)(rng1)
  }

  def mapViaFlatMap[A,B](ra:Rand[A])(f: A => B):Rand[B] = flatMap(ra)(a => (rng => (f(a),rng)))

  def map2ViaFlatMap[A,B,C](ra:Rand[A], rb:Rand[B])(f: (A,B) => C):Rand[C] = flatMap(ra)(a => {
    flatMap(rb)(b => (rng => (f(a,b), rng)))
  })


}