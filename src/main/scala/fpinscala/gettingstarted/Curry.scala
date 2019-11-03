//Another example of currying
//converts a function f of two arguments into a function of one argument that partially applies f

package gettingstarted

object Curry {

  def curry[A,B,C](f: (A, B) => C): A => (B => C) =
    (a: A) => (b: B) => f(a,b) //don't have a value for C, a and b so need to input the function f bringing in a and b

  curry

}
