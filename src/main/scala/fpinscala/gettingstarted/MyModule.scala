package gettingstarted

object MyModule { //Declares a singleton object, which simultaneously declares a class and its only instance

  def abs(n: Int): Int = //abs takes an integer and returns an integer
    if (n < 0) -n //Returns the negation of n if it's less than zero
  else n

  private def formatAbs(x: Int) = { //A private method can only be called by other members of MyModule
    val msg = "The absolute value of %d is %d." //A string with two placeholders for numbers marked as %d
    msg.format(x, abs(x)) //Replaces the two %d placeholders with x and abs(x) respectively.
    //format is a standard library method defined on String
  }

  def factorial(n: Int): Int = {
    def go(n: Int, acc: Int): Int = //inner function (local definition) local to body of another function
      if (n <= 0) acc
    else go(n-1, n*acc)

    go(n, 1)
  }

  private def formatFactorial(n: Int) = {
    val msg = "The factorial of %d is %d."
    msg.format(n, factorial(n))
  }

  def formatResult(name: String, n: Int, f: Int => Int) = { //f is required to be a function from Int to Int
    val msg = "The %s of %d is %d."
    msg.format(name, n, f(n))
  }

  def main(args: Array[String]): Unit = //Unit serves the same purpose as void in languages like Java or C
    println(formatFactorial(7))
    println(formatAbs(-42))




}
