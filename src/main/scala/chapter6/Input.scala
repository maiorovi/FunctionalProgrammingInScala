package chapter6

sealed trait Input {
}

case object Coin extends Input

case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

case class State[S, +A](run: S => (A,S)) {
//  def map[B](f: A => B): State[S, B] = State( state => {
//    val (value, newState) = run(newState)
//    (f(value), newState)
//  })

  def map[B](f: A => B): State[S, B] = flatMap(value => State.unit(f(value)))

  def map2[B,C](sb: State[S, B])(f: (A, B) => C): State[S, C] = flatMap(v1 => map(v2 => f(v1, v2)))

  def flatMap[B](f: A => State[S, B]): State[S, B] = f((state:A) => {
    val (value, newState) = run(state)
    value
  })

}

object State {

  def simulateMachine(inputs: List[Input]):State[Machine, (Int, Int)] = ???

  def unit[S,A](a:A):State[S, A] = State(state => (a, state))

}