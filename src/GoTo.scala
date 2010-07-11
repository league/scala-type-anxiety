import scala.util.continuations._
import scala.collection.mutable.HashMap

object GoTo {
  val labelMap = new HashMap[String, Unit=>Unit]

  // Seems to be needed to translate the one-armed 'if'
  implicit def unit2cps(u: Unit): Unit @suspendable =
    shiftUnit0[Unit,Unit]()

  def label(name:String) =
    shift { k:(Unit=>Unit) => labelMap += name -> k; k() }

  def goto(name:String) =
    shift { k:(Unit=>Unit) => labelMap(name)() }

  def main(args: Array[String]) =
    reset {
      var i = 0
      println("Hello world!")
      label("loop")
      println(i)
      i = i + 1
      if(i < 20) goto("loop")
      println("Done.")
    }
}
