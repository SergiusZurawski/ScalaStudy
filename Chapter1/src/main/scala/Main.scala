//https://github.com/fpinscala/fpinscala/


object Main {
  def main(args: Array[String]): Unit = {
    println("Hello, world!")
    println("Hello, THOMAS")
    val fibonacci = new Fibonacci()
    val poli = new PolimopicFuncition()
    //println(fibonacci.fiv(3))
    //println(fibonacci.fibNotTail(10))
    //println(fibonacci.fib(18))
//    println(fibonacci.fibNoIndex(18))
    println(poli.isSortedInt(Array(1, 2, 3)))
    println(poli.isSortedInt(Array(2, 2, 3)))
    println(poli.isSortedInt(Array(3, 2, 3)))

  }
}

class Cafe {
  var myVar: String = "From Cafee"
  def printItSelf(): String = {
    myVar
  }

  def abs(n: Int): Int =
    if (n < 0) -n
    else n

  def formatAbs(x: Int) = {
    val msg = "The absolute value of %d is %d"
    msg.format(x, abs(x))
  }
}

class PolimopicFuncition
{
//  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean ={
//    //as.
//  }
//
//  def isSortedInt(as: Array[Int], ordered: (Int, Int) => Boolean): Boolean ={
//
//  }

  def isSortedInt(as: Array[Int]): Boolean ={
    def loop(as: Array[Int], index: Int, previous: Int ): Boolean = {

      if(as.length <= index)
        return true

      val current = as(index)
      if(current < previous)
        return false

      loop(as, index+1, current)

    }

    if(as.length < 1) {
      false
    }


    return loop(as, 1, as(0))
  }

//  def isSortedIntNotRecursive(as: Array[Int]): Boolean ={
//    var item1: Int = 0
//    var item2: Int = 0
//    var index = 0;
//    for(item <-as)
//    {
//      index += 1;
//      if()
//    }
//  }

}

class Fibonacci {
  def fib(n: Int): Int = {
    @annotation.tailrec
    def loop(n: Int, index: Int, current: Int , previous: Int): Int = {
      if(index <= 0) {
        loop(n, index+1, 0, 0)
      }
      else if (index == 1){
        loop(n, index+1, 1+0, 1)
      }
      else if (index >= n)
        current
      else {
        println("loop")
        println("n: " + n)
        println("current: " + current)
        println("previous: " + previous)
        loop(n, index+1, current+previous, current)
      }
    }

    loop(n, 1, 0, 0)
  }

  def fibNoIndex(n: Int): Int = {
    @annotation.tailrec
    def loop(n: Int, current: Int , previous: Int): Int = {
      if(n <= 0) {
        current
      }
      else {
        loop(n-1,  current+previous, current)
      }
    }

    loop(n, 1, 0)
  }


  def fibNotTail(n: Int): Int = {
    def loop(n: Int): Int = {
      if(n <= 0) {
        0
      }
      else if(n == 1) {
        1
      }
      else {
        println("loop")
        println("n: " + n)
        println("result: ")
        loop(n - 1) + loop(n - 2)
      }
    }
    loop(n)
  }

}

class CurringClass {
  def curry[A, B, C](f: (A, B) => C) : A => (B => C) = {

    return (a: A) => (b: B) => f(a, b)
  }

  def generateFunction(a: Int) : Int => String = {
    return (a: Int) => a.toString
  }

  def curry[A, B, C](a: A, f:(A, B) => C): B => C = {
    return (b: B) => f(a, b)
  }

  def uncurry[A,B,C](f: A => B => C): (A, B) => C = {
    return (a: A, b: B) => f(a)(b)
  }

  def compose[A,B,C](f: B => C, g: A => B): A => C = {
    return (a: A) => f(g(a))
  }
}