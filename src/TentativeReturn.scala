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

  /* Unfortunately, using shift in a function passed to standard
     library fold/map/foreach will not work, because compiler needs to
     rewrite them into CPS. */
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
    println("Take TWO (streams)...")
    for(v <- multiUsingStream) {
      printf(" ====> %d\n", v)
    }
    println("Take THREE (continuations AND streams)...")
    for(v <- multiS) {
      printf(" ~~~~> %d\n", v)
    }
  }

  /* It occured to me after writing this example that it's exactly the
     same as a lazy stream. Just look at the ReturnThunk class: a pair
     of the element and a function to generate the next pair (wrapped
     in an option, so it may terminate). The only caveat is that you
     CAN'T write it in an imperative style... the entire future has to
     be in the function argument to Stream.cons. */

  def multiUsingStream = {
    println("This function returns tentative values,")
    println("but you can always ask for more.")
    Stream.cons(42, {
      println("Didn't like that? Okay, we're back again.")
      Stream.cons(99, {
        println("Let's try a range...")
        (1 to 5 toStream) append {
          println("Still not good enough? Well I'm out of ideas!")
          Stream.Empty
        }})})}

  /* Or, we can have a hybrid. Use shift/reset so we can have
     imperative tentative return statements, but do away with the
     custom ReturnThunk class in favor of Stream. */

  def produceS [A] (value: A): Unit @cps[Stream[A]] =
    shift {
      k: (Unit => Stream[A]) =>
        Stream.cons(value, k())
    }

  def multiS =
    reset {
      println("This function returns tentative values,")
      println("but you can always ask for more.")
      produceS(42)
      println("Didn't like that? Okay, we're back again.")
      produceS(99)
      println("Let's try a range...")
      foreach(1, 5, produceS)
      println("Still not good enough? Well I'm out of ideas!")
      Stream.Empty
    }

}
