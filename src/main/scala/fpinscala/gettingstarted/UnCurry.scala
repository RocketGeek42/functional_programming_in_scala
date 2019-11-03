//Implement Uncurry, which reverses the transformation of curry
package gettingstarted

object Uncurry {

  def uncurry[A,B,C](f: A => B => C): (A, B) => C =
    (a: A, b: B) => f(a)(b) //f(a)(b), f(a) returns a function

  uncurry

}
