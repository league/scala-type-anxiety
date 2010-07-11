import scala.util.Random

object ForComprehension {

  /* It's helpful to understand the translation of the for()
     statement in terms of flatMap and map. */

  def s1 = for(i <- 1 to 4; j <- 1 to i; k <- 1 to j) yield { i*j*k }

  def s2 =
    (1 to 4).flatMap { i =>
      (1 to i).flatMap { j =>
        (1 to j).map { k =>
          i*j*k }}}

  /* Consider operations that might work or fail */

  def getInt = Random.nextInt(4) match {
    case 0 => None
    case n => Some(n)
  }

  def main(args: Array[String]) {
    for(i <- 1 to 12) {
      val sum = for(x <- getInt; y <- getInt; z <- getInt) yield { x+y+z }
      println(sum)
    }
  }
}
