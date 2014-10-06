package fpinscala.datastructures

sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
case class Cons[+A](head: A, tail: List[A]) extends List[A] // Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`, which may be `Nil` or another `Cons`.

object List { // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int = ints match { // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x,xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  val x = List(1,2,3,4,5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h,t) => Cons(h, append(t, a2))
    }


  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x,y) => x + y)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar


  // 3.2
  def tail[A](l: List[A]): List[A] = {
    l match {
      case Nil => Nil
      case Cons(_, t) => t
    }
  }

  // 3.3
  def setHead[A](l: List[A], h: A): List[A] = {
    l match {
      case Nil => sys.error("setHead of empty list")
      case Cons(_, t) => Cons(h, tail(l))
    }
  }

  // 3.4
  def drop[A](l: List[A], n: Int): List[A] = {
    if (n <= 0) Nil
    else l match {
      case Nil => Nil
      case Cons(_, t) => drop(t, n - 1)
    }
  }

  // 3.5
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] =
    l match {
      case Nil => Nil
      case Cons(h, t) => {
        if (f(h)) dropWhile(t, f)
        else l
      }
    }

  // 3.6
  def init[A](l: List[A]): List[A] =
    l match {
      case Nil => Nil
      case Cons(_, Nil) => Nil
      case Cons(h, t) => Cons(h, init(t))
    }

  def foldRight[A,B](l: List[A], z: B)(f: (A, B) => B): B =
    l match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  // 3.9
  def length[A](l: List[A]): Int =
    foldRight(l, 0)((_, acc) => 1 + acc)

  // 3.11
  def sum3(ns: List[Int]) =
    foldLeft(ns, 0)(_ + _)

  // 3.11
  def product3(ns: List[Double]) =
    foldLeft(ns, 1.0)(_ * _)

  // 3.11
  def length2[A](l: List[A]): Int = {
    foldLeft(l, 0)((acc, _) => acc + 1)
  }

  // 3.12
  def reverse[A](l: List[A]): List[A] =
    foldLeft(l, List[A]())((acc, h) => Cons(h, acc))

  // 3.10
  @annotation.tailrec
  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = {
    l match {
      case Nil => z
      case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
    }
  }

  // 3.13
  def foldRightViaFoldLeft[A,B](l: List[A], z: B)(f: (A, B) => B): B =
    foldLeft(reverse(l), z)((acc, h) => f(h, acc))

  // 3.13
  def foldLeftViaFoldRight[A,B](l: List[A], z: B)(f: (B, A) => B): B =
    foldRight(l, (b:B) => b)((a,g) => b => g(f(b,a)))(z)

  // 3.14
  def append2[A](a1: List[A], a2: List[A]): List[A] =
    foldRight(a1, a2)(Cons(_, _))

  // 3.15
  def concatenate[A](ls: List[List[A]]): List[A] =
    foldRight(ls, Nil:List[A])(append)

  // 3.16
  def plusOne(l: List[Int]): List[Int] =
    foldRight(l, Nil:List[Int])((h, t) => Cons(1 + h, t))

  // 3.17
  def doubleToString(l: List[Double]) : List[String] =
    foldRight(l, Nil:List[String])((h, t) => Cons(h.toString, t))

  // 3.18
  def map[A,B](l: List[A])(f: A => B): List[B] =
    foldRightViaFoldLeft(l, Nil:List[B])((h, t) => Cons(f(h), t))

  // 3.19
  def filter[A](l: List[A])(f: A => Boolean): List[A] =
    foldRightViaFoldLeft(l, Nil:List[A])((h, t) => if (f(h)) Cons(h, t) else t)

  // 3.20
  def flatMap[A,B](l: List[A])(f: A => List[B]): List[B] =
    concatenate(map(l)(f))
    
  // 3.21
  def filterWithFlatMap[A](l: List[A])(f: A => Boolean): List[A] =
    flatMap(l)(a => if (f(a)) List(a) else Nil)

  // 3.22
  def addArrays[A](a1: List[Int], a2: List[Int]): List[Int] =
    (a1, a2) match {
      case (Nil, _) => Nil
      case (_, Nil) => Nil
      case (Cons(ha, ta), Cons(hb, tb)) => Cons(ha + hb, addArrays(ta, tb))
    }

  // 3.23
  def zipWith[A,B,C](a: List[A], b: List[B])(f: (A, B) => C): List[C] =
    (a, b) match {
      case (Nil, _) => Nil
      case (_, Nil) => Nil
      case (Cons(ha, ta), Cons(hb, tb)) => Cons(f(ha, hb), zipWith(ta, tb)(f))
    }

  // 3.24
  @annotation.tailrec
  def hasSubsequence[A](l: List[A], sub: List[A]): Boolean = {
    def startsWith(l: List[A], prefix: List[A]): Boolean =
      (l, prefix) match {
        case (_, Nil) => true
        case (Cons(h, t), Cons(hp, tp)) if h == hp => startsWith(t, tp)
        case _ => false
      }

    l match {
      case Nil => false
      case Cons(h, t) => {
        if (startsWith(l, sub)) true
        else hasSubsequence(t, sub)
      }
    }
  }
}

object Lists {
  def main(args: Array[String]): Unit = {
    val x = List(1,2,3,4,5)
    val y = List.foldRight(List(1,2,3), Nil:List[Int])(Cons(_,_))
    val z = List.foldRightViaFoldLeft(List(1,2,3), Nil:List[Int])(Cons(_,_))
    //println(List.foldLeftViaFoldRight(List(1,2,3), 0)(_ + _))

    //println(List.append(List(1,2,3), List(4,5)))
    //println(List.append2(List(1,2,3), List(4,5)))
    //println(List.concatenate(List(List(1,2,3), List(4), List(5,6))))

    //println(List.plusOne(x))

    //println(List.doubleToString(List(1.5, 2.0, 3.3)))

    //println(List.map(x)(_ * 2))

    //println(List.filter(x)(_ % 2 == 0))

    //println(List.flatMap(List(1,2,3))(i => List(i,i)))

    //println(List.filterWithFlatMap(x)(_ % 2 == 0))

    //println(List.addArrays(List(1,2,3), List(4,5,6)))

    //println(List.zipWith(List(1,2,3), List(4,5,6))(_ * _))

    println(List.hasSubsequence(x, List(1,2)))
    println(List.hasSubsequence(x, List(1)))
    println(List.hasSubsequence(x, List(4,5)))
    println(List.hasSubsequence(x, List(3,2)))
    println(List.hasSubsequence(x, List(1,2,4)))
    println(List.hasSubsequence(List(1,1,2,3), List(1,2)))

    /*
    List.reverse(List(1,2,3))
    foldLeft(Cons(1, Cons(2, Cons(3, Nil))), Nil)((as: List[A], a: A) => Cons(a, as))
    foldLeft(Cons(2, Cons(3, Nil)), Cons(1, Nil))((as: List[A], a: A) => Cons(a, as))
    foldLeft(Cons(3, Nil), Cons(2, Cons(1, Nil)))((as: List[A], a: A) => Cons(a, as))
    foldLeft(Nil, Cons(3, Cons(2, Cons(1, Nil))))((as: List[A], a: A) => Cons(a, as))
    Cons(3, Cons(2, Cons(1, Nil)))
    */
  }
}
