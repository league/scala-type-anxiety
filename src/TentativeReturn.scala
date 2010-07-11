import scala.util.continuations._

object TentativeReturn {

  implicit def unit2cps(u: Unit): Unit @suspendable =
    shiftUnit0[Unit,Unit]()

  case class ReturnThunk[A]
    (value: A,
     proceed: Unit => Option[ReturnThunk[A]])

  def produce [A] (value: A): Unit @cps[Option[ReturnThunk[A]]] =
    shift {
      k: (Unit => Option[ReturnThunk[A]]) =>
        Some(ReturnThunk(value, k))
    }

  /* Unfortunately, using shift in a higher-order function passed to
     standard library fold/map/foreach will not work, because compiler
     needs to rewrite them into CPS. */
  def foreach[B](i: Int, n: Int, body: Int => Unit @cps[B]): Unit @cps[B] =
    if(i < n) {
      body(i)
      foreach(i+1, n, body)
    }

  def multi =
    reset {
      println("This function returns tentative values,")
      println("but you can always ask for more.")
      produce(42)
      println("Didn't like that? Okay, we're back again.")
      produce(99)
      println("Let's try a range...")
      foreach(1, 5, produce)
      println("Still not good enough? Well I'm out of ideas!")
      None
    }

  def main(args: Array[String]) {
    var thunk : Option[ReturnThunk[Int]] = multi
    while(thunk.isDefined) {           // somewhat ugly Option-hacking
      printf(" ----> %d\n", thunk.get.value)
      thunk = thunk.get.proceed()
    }
  }
}
