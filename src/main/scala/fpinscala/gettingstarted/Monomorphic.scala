package gettingstarted

object Monomorphic {

  //find a string in an array
  def findFirst(ss: Array[String], key: String): Int = {
    @annotation.tailrec
    def loop(n: Int): Int =
      if (n >= ss.length) -1 //if n is past the end of the array, return -1, indicating the key doesn't exist in the array
    else if (ss(n) == key) n //ss(n) extracts the nth element of the array ss.  If the element at n is equal to the key, return n
    else loop(n + 1) //otherwise increment n and keep looking

    loop(0) //start at the very first element of the array
  }

}
