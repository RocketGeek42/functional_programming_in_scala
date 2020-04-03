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


  //5.7 Implement map, filter, append, and flatMap using foldRight. The append method should be non-strict in its argument
  def map[B](f: A => B): Stream[B] = ???
  def filter(pred: A => Boolean): Stream[A] = ???
  def append[AA >: A](as: Stream[AA]): Stream[AA] = ???
  def flatMap[B](f: A => Stream[B]): Stream[B] = ???

  //5.8 Generalize ones slightly to the function constant, which returns an infinite Stream of a given value
  def constant[A](a: A): Stream[A] = ???


  //5.9 Write a function that generates an infinite stream of integers, starting from n, then n + 1, n+ 2 and so on.
  def from(n: Int): Stream[Int] = ???


}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]