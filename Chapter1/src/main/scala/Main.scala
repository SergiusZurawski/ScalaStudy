object Main {
  def main(args: Array[String]): Unit = {
    println("Hello, world!")
    println("Hello, THOMAS")
    val cafe = new Cafe()
    println(cafe.printItSelf())
  }
}

class Cafe {
  var myVar: String = "From Cafee"
  def printItSelf(): String = {
    myVar
  }
}