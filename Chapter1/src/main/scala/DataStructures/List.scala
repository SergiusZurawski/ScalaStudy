package DataStructures

sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
/* Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
which may be `Nil` or another `Cons`.
 */
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List { // `List` companion object. Contains functions for creating and working with lists.
  println("Hello from LIST!!!");
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
  println("X IS : " +  x);
  def printX(): Int = {
    return x
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h,t) => Cons(h, append(t, a2))
    }

  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x,y) => x + y)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar


  def tail[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(head, tail) => tail
  }

  println("tail ----:")
  println(tail(List(1,2,3,4)))

  def setHead[A](l: List[A], h: A): List[A] = l match {
    case Nil => Nil
    case Cons(head, tail) => Cons(h, tail)
  }

  println("setHead ----:")
  println(setHead(List(1,2,3,4), 0))

  def drop[A](l: List[A], n: Int): List[A] = l match {
    case Nil => sys.error("drop on empty list")
    case xs if n <= 0 => xs
    case Cons(x, xs) => drop(xs, n-1)
  }

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Nil => Nil
    case Cons(x, xs) if !f(x) => println("if case"+ xs);dropWhile(xs, f)
    case _ => l
  }

  println("dropWhile ----:")
  println(dropWhile(List(1,2,3,4), (x:Int) => x%2 == 0))

  def dropFilter[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Nil => Nil
    case Cons(x, xs) if !f(x) => println("if case"+ xs);dropFilter(xs, f)
    case Cons(x, xs) => println("defalut case"+ xs); Cons(x, dropFilter(xs,f))
  }

  println("dropFilter ----:")
  println(dropFilter(List(1,2,3,4), (x:Int) => x%2 == 0))


  def init[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(head, Nil) => Nil
    case Cons(x, xs) => Cons(x, init(xs))
  }

  println("init ----:")
  println(init(List(1,2,3,4)))

  def length[A](l: List[A]): Int = ???

  def foldLeftImperfect[A,B](l: List[A], z: B)(f: (B, A) => B): B =  {
    @annotation.tailrec
    def tailRec(l: List[A], z: B, sum: B): B = l match {
      case Nil => sum
      case Cons(x, xs) => tailRec(xs, z, f(sum, x))
    }
    tailRec(l, z, z)
  }

  @annotation.tailrec
  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B =  l match{
    case Nil => z
    case Cons(head, tail) => foldLeft(tail, f(z, head))(f)
  }

  println("foldLeft ----:")
  println(foldLeftImperfect(List(1,2,3,4), 0)((x, y) => x + y))
  println(foldLeftImperfect(List(1,2,3,4), 1)((x, y) => x * y))
  println(foldLeft(List(1,2,3,4), 0)((x, y) => x + y))
  println(foldLeft(List(1,2,3,4), 1)((x, y) => x * y))
//  println(foldLeft(List(1,2,3,4), 0)((a, b) => a + b))
//  def foldLeftNotParametrized[A](l: List[A], z: A): A = {
//    @annotation.tailrec
//    def tailRec(l: List[A], z: A, sum: A): A = l match {
//      case Nil => z
//      case Cons(x, xs) => tailRec(xs, z, sum + x)
//    }
//    tailRec(l, z, z)
//  }

  def foldLeftNotParametrized(l: List[Int], z: Int): Int = {
    @annotation.tailrec
    def tailRec1(l: List[Int], z: Int, sum: Int): Int = l match {
      case Nil => z
      case Cons(x, xs) => tailRec1(xs, z, sum + x)
    }
    tailRec1(l, z, 0)
  }

  def sum3(l: List[Int]): Int = foldLeft(l, 0)(_ + _)
  def product3(l: List[Double]): Double = foldLeft(l, 1.0)(_ * _)
  def lenght3[A](l: List[A]): Int = foldLeft(l, 0)((a, b) => a + 1)

  println("CombinationOfFunction ----:")
  println(sum3(List(1,2,3)))
  println(product3(List(1,2,3)))
  println(lenght3(List(1,2,3)))

  def reverse[A](l: List[A]): List[A] = foldLeft[A, List[A]](l:List[A], List())((a,b) => Cons(b, a))
  println("reverse ----:")
  println(reverse(List(1,2,3)))

  def foldRightInFoldLeft[A,B](as: List[A], z: B)(f: (A, B) => B): B = foldLeft(as, z)((a, b) => f(b,a))
  def sum3(ns: List[Int]): List[Int] = foldRightInFoldLeft
  println("reverse ----:")
  println(reverse(List(1,2,3)))
  /***
    FoldRight
    def sum2(ns: List[Int]) = foldRight(ns, 0)((x,y) => x + y)
    sum2(List(1,2,3))
    Cons(x, xs) => f(x, foldRight(xs, z)(f))
    Cons(1, List(2,3,Nil)) => f(1, foldRight(List(2,3,Nil), 0)(f))
    Cons(2, List(3,Nil))   => f(2, foldRight(List(3,Nil),   0)(f))
    Cons(3, List(Nil))     => f(3, foldRight(List(Nil),     0)(f))

    f(1, foldRight(f(2, foldRight(f(3, foldRight(List(Nil)))

    f(1 + f(2 + f(3 + (Nill))))

    FoldLeft
    def sum2(ns: List[Int]) = FoldLeft(ns, 0)((x,y) => x + y)
    sum2(List(1,2,3))
    Cons(head, tail) => foldLeft(tail, f(z, head))(f)
    Cons(1, List(2,3,Nil)) => foldLeft(List(2,3,Nil), f(0, 1))(f)
    Cons(2, List(3,Nil))   => foldLeft(List(3,Nil),   f(1, 2))(f)
    Cons(3, List(Nil))     => foldLeft(List(Nil),     f(3, 3))(f)
    foldLeft(foldLeft(foldLeft(List(Nil),3 ),2) 1) 0
    fl(Nil + f(3 + f(2 + f(1 + 0)))
  */


  def map[A,B](l: List[A])(f: A => B): List[B] = ???
}