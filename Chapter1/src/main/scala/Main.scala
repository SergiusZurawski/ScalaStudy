object Main {
  def main(args: Array[String]): Unit = {
    println("Hello, world!")
    println("Hello, THOMAS")
    val fibonacci = new Fibonacci()
    //println(fibonacci.fiv(3))
    //println(fibonacci.fibNotTail(10))
    //println(fibonacci.fib(18))
    println(fibonacci.fibNoIndex(18))

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
/*
*
*  if(n <= 1) {
        println("n<=0")
        println(result)
        result
      }
      else if(n == 2) {
        println("n==1")
        result + 1
      }
      else {
        println("loop")
        println("n: " + n)
        println("result: " + result)
        loop(n-1,result + n)
      }*/