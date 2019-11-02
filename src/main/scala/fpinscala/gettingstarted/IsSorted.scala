//Implement IsSorted, which checks whether an Array[A] is sorted
//according to the given comparison function
package gettingstarted

object IsSorted {

  def IsSorted[A](as: Array[A], bt: (A, A) => Boolean): Boolean ={
    @annotation.tailrec
    def loop(n: Int): Boolean =
      if (n >= as.length - 1) true
    else if(bt(as(n), as(n + 1))) false
    else loop(n + 1)

    loop(0)
  }

}
