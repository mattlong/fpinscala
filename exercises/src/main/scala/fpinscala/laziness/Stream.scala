package fpinscala.laziness

import Stream._
trait Stream[+A] {

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h,t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }

  def exists(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }

  // Excercise 5.1
  def toListRecursive: List[A] = this match {
    case Cons(h, t) => h() :: t().toList
    case _ => List()
  }

  def toList: List[A] = {
    @annotation.tailrec
    def go(s: Stream[A], acc: List[A]): List[A] = s match {
      case Cons(h, t) => go(t(), h() :: acc)
      case _ => acc
    }
    go(this, List()).reverse
  }

  // Exercise 5.2
  def take(n: Int): Stream[A] = {
    if (n > 0) this match {
      case Cons(h, t) if n == 1 => cons(h(), empty)
      case Cons(h, t) => cons(h(), t().take(n-1))
      case _ => empty
    }
    else empty
  }

  def drop(n: Int): Stream[A] = {
    if (n > 0) this match {
      case Cons(h, t) => t().drop(n - 1)
      case _ => empty
    }
    else this
  }

  // Exercise 5.3
  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if p(h()) => cons(h(), t().takeWhile(p))
    case _ => empty
  }

  // Exercise 5.4
  def forAll(p: A => Boolean): Boolean =
    foldRight(true)((a, b) => p(a) && b)

  // Exercise 5.5
  def takeWhile_1(p: A => Boolean): Stream[A] =
    foldRight(empty[A])((h, t) =>
      if (p(h)) cons(h, t)
      else empty
    )

  // Exercise 5.6
  def headOption: Option[A] =
    foldRight(None: Option[A])((h, _) => Some(h))

  // Excercise 5.7
  def map[B](f: A => B): Stream[B] =
    foldRight(empty[B])((h, t) => cons(f(h), t))

  def filter[B](f: A => Boolean): Stream[A] =
    foldRight(empty[A])((h, t) =>
      if (f(h)) cons(h, t)
      else t
    )

  def append[B>:A](s: => Stream[B]): Stream[B] =
    foldRight(s)((h, t) => cons(h, t))

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(empty[B])((h, t) => f(h).append(t))

  // Excercise 5.13
  def mapViaUnfold[B](f: A => B): Stream[B] =
    unfold(this) {
      case Cons(h, t) => Some((f(h()), t()))
      case _ => None
    }

  def takeViaUnfold(n: Int): Stream[A] =
    unfold((this, n)) {
      case (Cons(h, t), n) if n == 1 => Some((h(), (empty, n-1)))
      case (Cons(h, t), n) if n > 0 => Some((h(), (t(), n-1)))
      case _ => None
    }

  def takeWhileViaUnfold(p: A => Boolean): Stream[A] =
    unfold(this) {
      case Cons(h, t) if (p(h())) => Some((h(), t()))
      case _ => None
    }

  def zipWithViaUnfold[B,C](b: Stream[B])(f: (A, B) => C): Stream[C] =
    unfold((this, b)) {
      case (Cons(ha, ta), Cons(hb, tb)) => Some((f(ha(), hb()), (ta(), tb())))
      case _ => None
    }

  def zipAll[B](s2: Stream[B]): Stream[(Option[A],Option[B])] =
    unfold((this, s2)) {
      case (Empty, Cons(hb, tb)) => Some(((None, Some(hb())), (empty, tb())))
      case (Cons(ha, ta), Empty) => Some(((Some(ha()), None), (ta(), empty)))
      case (Cons(ha, ta), Cons(hb, tb)) => Some(((Some(ha()), Some(hb())), (ta(), tb())))
      case _ => None
    }

  // Excercise 5.14
  def startsWith[B](s: Stream[B]): Boolean =
    zipAll(s).takeWhile((x) => !x._2.isEmpty).forAll {
      case (ha, hb) => ha == hb
    }

  // Exercise 5.15
  def tails: Stream[Stream[A]] =
    unfold(this) {
      case Empty => None
      case s => Some(s, s.drop(1))
    }.append(Stream())

  def hasSubsequence[A](s: Stream[A]): Boolean =
    tails.exists(_.startsWith(s))
}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))

  val ones: Stream[Int] = Stream.cons(1, ones)

  // Excercise 5.8
  def constant[A](a: A): Stream[A] = {
    lazy val tail: Stream[A] = Cons(() => a, () => tail)
    tail
  }

  // Excercise 5.9
  def from(n: Int): Stream[Int] = {
    cons(n, from(n + 1))
  }

  // Excercise 5.10
  def fibs(): Stream[Int] = {
    def go(f0: Int, f1: Int): Stream[Int] =
      cons(f0, go(f1, f0 + f1))
    go(0, 1)
  }

  // Exercise 5.11
  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] =
    f(z) match {
      case Some((a, s)) => cons(a, unfold(s)(f))
      case None => empty
    }

  // Exercise 5.12
  def fibsViaUnfold(): Stream[Int] =
    unfold((0, 1)){ case (f0, f1) => Some(f0, (f1, f0 + f1)) }

  def fromViaUnfold(n: Int): Stream[Int] =
    unfold(n)(n => Some(n, n+1))

  def constantViaUnfold[A](a: A): Stream[A] =
    unfold(a)(_ => Some(a, a))

  val onesViaUnfold: Stream[Int] = unfold(1)(x => Some(x, x))
}

object Streams {
  def main(args: Array[String]): Unit = {
    val x = Stream(1, 2, 3, 4)
    val y = Stream(5, 6, 7, 8)
    val z = Stream(9, 10, 11, 12, 13, 14)
    println(x.toList)
    println(y.toList)
    println(z.toList)
    println()

    //println(x.take(2).toList)

    //println(x.drop(0).toList)
    //println(x.drop(1).toList)
    //println(x.drop(2).toList)
    //println(x.drop(3).toList)
    //println(x.drop(4).toList)

    //println(x.takeWhile(_ < 3).toList)

    //println(x.forAll(_ < 5))
    //println(x.forAll(_ % 2 == 0))

    //println(x.headOption)

    //println(x.map(_ + 3).toList)

    //println(x.filter(_ % 2 == 1).toList)

    //println(x.append(y).toList)

    //println(x.flatMap(x => Stream(x, x)).toList)

    //println(Stream.constant(5).take(3).toList)

    //println(Stream.from(100).take(10).toList)

    //println(Stream.fibs().take(10).toList)

    //val s = Stream.unfold(1)(x => {
    //  println("state = " + x.toString)
    //  if (x < 10) Some((x, x + 1)) else None
    //})

    //println(Stream.fibsViaUnfold().take(10).toList)

    //println(Stream.fromViaUnfold(100).take(10).toList)

    //println(Stream.constantViaUnfold(5).take(3).toList)

    //println(Stream.onesViaUnfold.take(7).toList)

    //println(x.mapViaUnfold(_ + 3).toList)

    //println(x.takeViaUnfold(2).toList)

    //println(y.zipAll(z).toList)

    //println(x.startsWith(Stream(1,2)))
    //println(x.startsWith(Stream(2,2)))

    //println(x.tails.toList.map(_.toList))

    println(x.hasSubsequence(Stream(2, 3, 4)))
    println(x.hasSubsequence(Stream(3, 2, 4)))
  }
}
