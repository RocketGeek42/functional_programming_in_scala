sealed trait Stream[+A] {

  //5.1 Write a function to convert a Stream to a List
  def toList: List[A] = ???


  //5.2 Write the function take(n) for returning the first n elements of a Stream, and drop(n) for skipping the first n elements of a Stream.
  def take(n: Int): Stream[A] = ???
  def drop(n: Int): Stream[A] = ???

  //5.3 Write the function takeWhile for returning all starting elements of a Stream that match the given predicate.
  def takeWhile(p: A => Boolean): Stream[A] = ???

  //5.4 Implement forAll, which checks that all elements in the Stream match a given predicate.  Your implementation should terminate the traversal as soon as it encounters a nonmatching value.
  def forAll(p: A => Boolean): Boolean = ???

  //5.5 Use foldRight to implement takeWhile


  //5.6 Implement headOption using foldRight.


}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]