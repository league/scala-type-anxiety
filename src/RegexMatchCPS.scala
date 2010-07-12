object RegexMatchCPS {

  class Regex
  case class Literal(s: String) extends Regex
  case class Concat(r1: Regex, r2: Regex)
       extends Regex
  case class Choice(r1: Regex, r2: Regex)
       extends Regex
  case class Star(r: Regex) extends Regex

  def accept (regex: Regex,
              chars: Seq[Char],
              k: Seq[Char] => Boolean): Boolean = {
    printf("%s |- %s\n", regex, chars)
    regex match {
      case Literal(expected) =>
        if(chars.take(expected.length).sameElements(expected))
          k(chars.drop(expected.length))
        else false
      case Concat(regex1, regex2) =>
        accept(regex1, chars, remaining =>
          accept(regex2, remaining, k))
      case Choice(regex1, regex2) =>
        accept(regex1, chars, k) || accept(regex2, chars, k)
      case Star(repeatable) =>
        k(chars) ||
        accept(repeatable, chars, remaining =>
          accept(regex, remaining, k))
    }
  }

  def complete (remaining: Seq[Char]): Boolean = {
    printf("Complete? '%s'\n", remaining)
    remaining.length == 0
  }

  def showAccept(regex: Regex, chars: String, expected: Boolean) {
    println
    val result = accept(regex, chars, complete)
    printf("%s => %b [%s]\n", chars, result,
           if(result == expected) "OK" else "error")
  }

  val regex1 = Concat(Star(Literal("ab")), Literal("abc"))

  val regex2 = Choice(Star(Literal("ab")),
                      Concat(Literal("a"), Star(Literal("ba"))))

  def main(args: Array[String]) {
    showAccept(Literal("abc"), "abc", true)
    showAccept(regex1, "abababc", true)
    showAccept(regex1, "abababd", false)
    showAccept(regex2, "abababa", true)
  }
}
