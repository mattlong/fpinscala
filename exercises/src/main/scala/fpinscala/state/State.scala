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

  // Excercise 6.1
  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (n, r) = rng.nextInt
    (if (n < 0) -(n + 1) else n, r)
  }

  // Excercise 6.2
  def double(rng: RNG): (Double, RNG) = {
    val (n, r) = nonNegativeInt(rng)
    (n / (Int.MaxValue.toDouble + 1), r)
  }

  // Excercise 6.5
  def doubleViaMap: Rand[Double] =
    map(nonNegativeInt)(_ / (Int.MaxValue.toDouble + 1))

  // Excercise 6.3
  def intDouble(rng: RNG): ((Int,Double), RNG) = {
    val (n, r1) = rng.nextInt
    val (d, r2) = double(r1)
    ((n, d), r2)
  }

  def doubleInt(rng: RNG): ((Double,Int), RNG) = {
    val ((n, d), r) = intDouble(rng)
    ((d, n), r)
  }

  def double3(rng: RNG): ((Double,Double,Double), RNG) = {
    val (d1, r1) = double(rng)
    val (d2, r2) = double(r1)
    val (d3, r3) = double(r2)
    ((d1, d2, d3), r3)
  }

  // Excercise 6.4
  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    @annotation.tailrec
    def go(count: Int, r: RNG, l: List[Int]): (List[Int], RNG) = {
      if (count == 0)
        (l, r)
      else {
        val (i, r1) = r.nextInt
        go(count - 1, r1, i :: l)
      }
    }
    go(count, rng, List())
  }

  // Excercise 6.6
  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    rng => {
      val (a, rngA) = ra(rng)
      val (b, rngB) = rb(rngA)
      (f(a, b), rngB)
    }

  def both[A,B](ra: Rand[A], rb: Rand[B]): Rand[(A,B)] =
    map2(ra, rb)((_, _))

  def intDoubleViaBoth: Rand[(Int, Double)] =
    both(int, double)

  // Excercise 6.7
  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
    fs.foldRight(unit(List[A]()))((f, acc) => map2(f, acc)(_ :: _))

  def intsViaSequence(count: Int): Rand[List[Int]] =
    sequence(List.fill(count)(int))

  // Exercise 6.8
  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] =
    rng => {
      val (a, ra) = f(rng)
      g(a)(ra)
    }

  def nonNegativeLessThan(n: Int): Rand[Int] =
    flatMap(int)(i => {
      val mod = i % n
      if (i + (n - 1) - mod >= 0) unit(mod)
      else nonNegativeLessThan(n)
    })

  // Exercise 6.9
  def mapViaFlatMap[A,B](s: Rand[A])(f: A => B): Rand[B] =
    flatMap(s)(a => unit(f(a)))

  def map2ViaFlatMap[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    flatMap(ra)(a => map(rb)(b => f(a, b)))

  def doubleViaMapViaFlatMap: Rand[Double] =
    mapViaFlatMap(nonNegativeInt)(_ / (Int.MaxValue.toDouble + 1))
}

object Randos {
  def main(args: Array[String]): Unit = {
    val s = new RNG.Simple(1)
      //println(RNG.nonNegativeInt(s)._1)
      //println(RNG.double(s)._1)
      //println(RNG.doubleViaMap(s)._1)
      //println(RNG.double3(s)._1)
      //println(RNG.ints(5)(s)._1)
      //println(RNG.intDoubleViaBoth(s)._1)
      //println(RNG.intDouble(s)._1)

      //println(RNG.ints(5)(s)._1)
      //println(RNG.intsViaSequence(5)(s)._1)
      //println(RNG.nonNegativeLessThan(6)(s)._1)
      //println(RNG.nonNegativeLessThan(90)(s)._1)

      println(RNG.doubleViaMap(s)._1)
      println(RNG.doubleViaMapViaFlatMap(s)._1)

  }
}

case class State[S,+A](run: S => (A, S)) {
  // Excercise 6.10
  def map[B](f: A => B): State[S, B] =
    flatMap(a => unit(f(a)))

  def map2[B,C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    flatMap(a => sb.map(b => f(a, b)))

  def flatMap[B](f: A => State[S, B]): State[S, B] =
    s => {
      val (a, s1) = run(s)
      f(a).run(s1)
    }
}

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object State {
  def unit[S, A](a: A): State[S, A] =
    State(s => (a, s))

  type Rand[A] = State[RNG, A]

  def modify[S](f: S => S): State[S, Unit] = for {
    s <- get // Gets the current state and assigns it to `s`.
    _ <- set(f(s)) // Sets the new state to `f` applied to `s`.
  } yield ()

  def get[S]: State[S, S] = State(s => (s, s))

  def set[S](s: S): State[S, Unit] = State(_ => ((), s))
}

object Candy {
  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = {

  }
}
