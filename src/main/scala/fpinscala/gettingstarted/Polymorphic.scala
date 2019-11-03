package gettingstarted

object Polymorphic {

  //function to find an element in an array
  def findFirst[A](as: Array[A], p: A => Boolean): Int = { //instead of hardcoding String, take a Type A as a parameter
    //instead of hardcoding an equality check, take a function with which to test each element of the array
    @annotation.tailrec
    def loop(n: Int): Int =
      if (n >= as.length) -1
    else if (p(as(n))) n //if the function p matches the current element, return its index in the array
    else loop(n + 1)

    loop(0)
  }

}
