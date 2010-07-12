object StateTransformer {

  case class ST[S,A](exec: S => (S,A)) {
    def flatMap[B] (f: A => ST[S,B]): ST[S,B] =
      ST { s0 =>
        val (s1, a) = exec(s0)
        f(a).exec(s1)
        }

    def map[B] (f: A => B) (implicit unit: B => ST[S,B]) : ST[S,B] =
      flatMap { x => unit(f(x)) }
  }

  implicit def unitST[S,A] (x: A): ST[S,A] =
    ST { s0 => (s0, x) }

  def init[S]: ST[S,S] =
    ST { s0 => (s0, s0) }

  def update[S] (g: S => S): ST[S,Unit] =
    ST { s0 => (g(s0), ()) }

  import TreeNumbering.{Tree,Leaf,Branch}

  def rightMost[A] (root: Tree[A]): A =
    root match {
      case Leaf(x) => x
      case Branch(left, right) => rightMost(right)
    }

  def inject[A] (root: Tree[A], cur: A): (Tree[A], A) =
    root match {
      case Leaf(old) => (Leaf(cur), old)
      case Branch(left, right) =>
        val (t1, last1) = inject(left, cur)
        val (t2, last2) = inject(right, last1)
        (Branch(t1,t2), last2)
    }

  def rotate[A] (root: Tree[A]) = {
    val (t1, _) = inject(root, rightMost(root))
    t1
  }

  def injectST[A] (root: Tree[A]): ST[A, Tree[A]] =
    root match {
      case Leaf(old) =>
        for(cur <- init[A];
            u <- update[A](_ => old))
          yield Leaf(cur)
      case Branch(left, right) =>
        for(t1 <- injectST(left);
            t2 <- injectST(right))
          yield Branch(t1,t2)
    }
}
