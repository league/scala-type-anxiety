import scala.collection.mutable.HashMap
import scala.util.Random
import scala.util.continuations._

object HumanInteract {

  /* This simulates the idea of a continuation-based web framework.
   * When we need to pause for human interaction, we store the current
   * continuation on the server, keyed to a UUID. When user submits
   * that form, use the UUID to resume the continuation.
   */

  type UUID = Int                       // good enough for example
  def uuidGen: UUID = Random.nextInt

  type Data = Int
  val sessions = new HashMap[UUID, Data=>Unit]

  def ask(prompt: String): Data @cps[Unit] =
    shift {
      k: (Data => Unit) => {
        val id = uuidGen
        printf("%s\nrespond with: submit(0x%x, ...)\n",
               prompt, id)
        sessions += id -> k
      }
    }

  def submit(id: UUID, data: Data) =
    sessions(id)(data)

  /* Here we can get nice straight-line code, even though it is actually
   * interrupted for user interaction.
   */
  def go =
    reset {
      val first = ask("Please give me a number")
      val second = ask("Please enter another number")
      printf("The sum of your numbers is: %d\n", first + second)
    }
}
