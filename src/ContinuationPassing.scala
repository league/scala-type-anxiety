object ContinuationPassing {

  def plus [A] (x: Int, y: Int, k: Int=>A): A = k(x+y)

  def times [A] (x: Int, y: Int, k: Int=>A): A = k(x*y)

  def less [A] (x: Int, y: Int, kt: => A, kf: => A): A =
    if(x < y) kt else kf

  def factorial [A] (n: Int, k: Int => A): A =
    less(n, 2, k(1),
         plus(n, -1, (m:Int) =>
           factorial(m, (f:Int) =>
             times(n, f, k))))

  def showOneResult [A] (n: Int, k: => A): A =
    factorial(n, (f:Int) => {
      printf("%d! = %d\n", n, f)
      k
    })

  def loopThruRange [A] (n: Int, m: Int, k: => A): A =
    less(n, m,
         showOneResult(n,
                       plus(n, 1, (n1:Int) =>
                         loopThruRange(n1, m, k))),
         k)

  def main(args: Array[String]) =
    loopThruRange(3, 10, {})
}
