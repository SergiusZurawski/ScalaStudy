object Main {
  def main(args: Array[String]): Unit = {
    println("Hello, world!")
    println("Hello, THOMAS")
    val cafe = new Cafe()
    println(cafe.printItSelf())

    println(cafe.formatAbs(-42))
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