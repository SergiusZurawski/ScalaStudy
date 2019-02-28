import util.control.Breaks._

object Main {
  def main(args: Array[String]): Unit = {
    println("Hello, world!")
    println("Hello, THOMAS")
    val fibonacci = new Fibonacci()
    //println(fibonacci.fiv(3))
    //println(fibonacci.fibNotTail(10))
    //println(fibonacci.fib(18))
    //println(fibonacci.fibNoIndex(18))
    val parametrizedPolimorphism = new PolimopicFuncition();
//    println(parametrizedPolimorphism.isSortedIntNotRecursive(Array(3, 2, 3)))
//    println(parametrizedPolimorphism.isSortedIntNotRecursive(Array(1, 2, 3)))
//    println(parametrizedPolimorphism.isSortedIntNotRecursive(Array(2, 2, 3)))
//    println(parametrizedPolimorphism.isSortedIntNotRecursive(Array(1, 5, 3)))

    println(parametrizedPolimorphism.isSortedInt(Array(1, 2, 3)))

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
  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean ={
    //as.
    return false
  }

  def isSortedInt(as: Array[Int], ordered: (Int, Int) => Boolean): Boolean ={
    return false
  }

  def isSortedInt(as: Array[Int]): Boolean ={
    def loop(as: Array[Int], index: Int,  prvious: Int, current: Int, state: Boolean): Boolean = {


    }

    if (as.length < 1)
      false

    return
  }

  def isSortedIntNotRecursive(as: Array[Int]): Boolean ={
    var previous: Int = 0
    var current: Int = 0
    var index = 0;
    for(item <-as)
    {
      index += 1;
      current = item;
      breakable {
        if (index == 1) {
          previous = current
          break
        }
      }
      if(current < previous)
        return false

      previous = current
    }
    return true
  }

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
