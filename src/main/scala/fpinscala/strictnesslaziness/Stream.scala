sealed trait Stream[+A] {

  //5.1 Write a function to convert a Stream to a List
  def toList: List[A] = ???


  //5.2 Write the function take(n) for returning the first n elements of a Stream, and drop(n) for skipping the first n elements of a Stream.
  def take(n: Int): Stream[A] = ???
  def drop(n: Int): Stream[A] = ???


}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]