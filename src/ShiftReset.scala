import scala.util.continuations._

object ShiftReset {

  def doSomething0 = {
    val result = 1 + 2 * 3
    println(result)
  }

  def doSomething1 =
    reset {
      println("Ready?")
      val result = 1 + special * 3
      println(result)
    }

  def special =
    shift {
      k: (Int => Unit) =>
        println(99)
        "Gotcha!"
    }

  def doSomething2 = reset { 1 + tryMe * 3 }

  def tryMe =
    shift {
      k: (Int => Int) =>
        println(k(2))
        println(k(8))
        println(k(k(3)))
        42
    }

  def main(args: Array[String]) {
    doSomething0 // produces Unit
    println(doSomething1) // produces String
    println(doSomething2) // produces Int
  }
}
