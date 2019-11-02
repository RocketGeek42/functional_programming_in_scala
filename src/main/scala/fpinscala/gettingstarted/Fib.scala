//Write a recursive function to get the nth Fibonacci number
//The first two are 0 and 1, nth is always the sum of the previous two
package gettingstarted

object Fib {

  def fib(n: Int): Int = {
    @annotation.tailrec
    def loop(n: Int, prev_1: Int, prev_2: Int): Int =
      if (n <= 1) prev_1
    else loop(n-1, prev_2, prev_1 + prev_2)

    loop(n, 0, 1)
  }
}
