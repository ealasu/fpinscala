package fpinscala.state


trait RNG {
  def nextInt: (Int, RNG) // Should generate a random `Int`. We'll later define other functions in terms of `nextInt`.
}

object RNG {
  // NB - this was called SimpleRNG in the book text

  case class Simple(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL // `&` is bitwise AND. We use the current seed to generate a new seed.
      val nextRNG = Simple(newSeed) // The next state, which is an `RNG` instance created from the new seed.
      val n = (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
      (n, nextRNG) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.
    }
  }

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A,B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    var (v, rng) = rng.nextInt()
    if v == Int.MinValue {
      v = Int.MaxValue
    } else if v < 0 {
      v = -v
    }
    (v, rng)
  }

  def double(rng: RNG): (Double, RNG) = {
    val (v, rng) = rng.nextInt()
    val v = v.toDouble() / Int.MaxValue
    (v, rng)
  }
  
  def intDouble(rng: RNG): ((Int,Double), RNG) = {
    val (i, rng) = rng.nextInt()
    val (d, rng) = double(rng)
    ((i, d), rng)
  }

  def doubleInt(rng: RNG): ((Double,Int), RNG) = {
    val (d, rng) = double(rng)
    val (i, rng) = rng.nextInt()
    ((d, i), rng)
  }

  def double3(rng: RNG): ((Double,Double,Double), RNG) = {
    val (d1, rng) = double(rng)
    val (d2, rng) = double(rng)
    val (d3, rng) = double(rng)
    ((d1, d2, d3), rng)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    def intsInner(count: Int, rng: RNG, list: List[Int]): (RNG, List[Int]) {
      if (count == 0) {
        (rng, list)
      } else {
        val (v, rng) = rng.nextInt
        intsInner(count - 1, rng, v :: list)
      }
    }
    intsInner(count, rng, List())
  }

  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    rng => {
      val (a, rng2) = ra(rng)
      val (b, rng3) = rb(rng2)
      (f(a, b), rng3)
    }
  
  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = ???

  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] = ???
  
  
  // using map
  
  def doubleUsingMap(rng: RNG): (Double, RNG) =
    map(rng.nextInt)(_.toDouble / Int.MaxValue)
  
  def intDouble(rng: RNG): ((Int,Double), RNG) =
    map2(_.nextInt, double)((_, _))
    
  def 
 
}

case class State[S,+A](run: S => (A, S)) {
  def map[B](f: A => B): State[S, B] =
    ???
  def map2[B,C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    ???
  def flatMap[B](f: A => State[S, B]): State[S, B] =
    ???
}

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object State {
  type Rand[A] = State[RNG, A]
  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = ???
}
