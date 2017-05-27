package chapter6

sealed trait Input {
}

case object Coin extends Input

case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

case class State[S, +A](run: S => (A, S)) {
  //  def map[B](f: A => B): State[S, B] = State( state => {
  //    val (value, newState) = run(newState)
  //    (f(value), newState)
  //  })

  def map[B](f: A => B): State[S, B] = flatMap(value => State.unit(f(value)))

  def map2[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] = flatMap(v1 => map(v2 => f(v1, v2)))

  def flatMap[B](f: A => State[S, B]): State[S, B] = f((state: A) => {
    val (value, newState) = run(state)
    value
  })

}

object State {
  def update(input: Input): Machine => Machine = (machine: Machine) => machine match {
    case Machine(_, 0, _) => machine //if there is no candies ignore all inputs
    case Machine(false, candies, coins) => input match {
      case Coin => machine //inserting a coint ot unlocked machine does nothing
      case Turn => Machine(true, candies -1, coins) //
    }
      //inserting a coin
    case Machine(true, candies, coins) => input match {
      case Coin => Machine(false, candies, coins + 1) //unlock machine
      case Turn => machine // turning the knob on locked machine does nothing
    }
  }

  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = {
    State( machine => {
      inputs.foreach(inp => update(inp)(machine))
      ((machine.candies, machine.coins),machine)
    } )
  }

  def unit[S, A](a: A): State[S, A] = State(state => (a, state))

}