import scalaz._
import Scalaz._

object TreeNumbering {

  class Tree[A]
  case class Leaf[A](elem: A) extends Tree[A]
  case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

//  ___  ___  f            L1 L1           1="___"
//  |    \__  ___  d       L2 R3 L1        2="|  "
//  |         \__  ___e    L2 R4 R3 L1     3="\__"
//  |              \__c    L2 R4 R4 R3     4="   "
//  \__  ___  a            R3 L1
//       \__  ___  ___h    R4 R3 L1 L1
//            |    \__g    R4 R4 L2 R3
//            \__  b       R4 R4 R3

  val t1 = Branch(Branch(Leaf('f'),
                         Branch(Leaf('d'), Branch(Leaf('e'), Leaf('c')))),
                  Branch(Leaf('a'),
                         Branch(Branch(Leaf('h'), Leaf('g')), Leaf('b'))))

  def drawLines(path: String): Unit =
    path foreach {
      case 'L' => print("___")
      case 'l' => print("|  ")
      case 'R' => print("\\__")
      case 'r' => print("   ")
    }

  def drawTree[A](root: Tree[A], path: String): Unit =
    root match {
      case Leaf(elem) => {
        drawLines(path)
        println(elem)
      }
      case Branch(left, right) => {
        drawTree(left, path+"L")
        drawTree(right, path.toLowerCase()+"R")
      }
    }

  def number[A](root: Tree[A], seed: Int): (Tree[(A,Int)], Int) =
    root match {
      case Leaf(a) => (Leaf(a, seed), seed+1)
      case Branch(left, right) => {
        val (doneLeft, next) = number(left, seed)
        val (doneRight, last) = number(right, next)
        (Branch(doneLeft, doneRight), last)
      }
    }

  val (t2, _) = number(t1, 0)

  def numberSt[A](root: Tree[A]): State[Int, Tree[(A,Int)]] =
    root match {
      case Leaf(a) => for(s <- init[Int];
                          n <- modify[Int](1+_))
                      yield Leaf(a, s)
      case Branch(left, right) =>
        for(l <- numberSt(left);
            r <- numberSt(right))
        yield Branch(l,r)
    }

  val (_, t3) = numberSt(t1)(0)
}
