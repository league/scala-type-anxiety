import scalaz._
import Scalaz._

object TreeNumbering {

  class Tree[A]
  case class Leaf[A](a: A) extends Tree[A]
  case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

  def number[A](root: Tree[A], seed: Int): (Tree[(A,Int)], Int) =
    root match {
      case Leaf(a) => (Leaf(a, seed), seed+1)
      case Branch(left, right) => {
        val (doneLeft, next) = number(left, seed)
        val (doneRight, last) = number(right, next)
        (Branch(doneLeft, doneRight), last)
      }
    }

  val ex1 = Branch(Branch(Leaf('a'), Branch(Leaf('b'), Leaf('c'))),
                   Branch(Branch(Leaf('d'), Branch(Leaf('e'), Leaf('f'))),
                          Branch(Branch(Leaf('g'), Leaf('h')), Leaf('i'))))

  val ex2 = number(ex1, 1)

  def numberSt[A](root: Tree[A]): State[Int, Tree[(A,Int)]] =
    root match {
      case Leaf(a) => for(s <- init[Int];
                          n <- modify[Int](1+_))
                      yield Leaf(a, s+1)
      case Branch(left, right) =>
        for(l <- numberSt(left);
            r <- numberSt(right))
        yield Branch(l,r)
    }
}
