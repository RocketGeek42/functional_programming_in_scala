sealed trait Stream[+A]
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A] (hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }
  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))

  def headOption: Option[A] = this match {
    case Empty => None
    case Cons(h, t) => Some(h())
  }

  //5.1 Write a function to convert a Stream to a List
  def toList: List[A] = this match {
    case Empty => None
    case Cons(h, t) => h() :: t().toList
  }

  //5.2 Write the function take(n) for returning the first n elements of a Stream, and drop(n) for dropping the first n elements of a Stream.
  def take(n: Int): Stream[A] = this match {
    case Empty => Empty
    case Cons(h, t) if n > 0 => Cons(h, () => t.take(n - 1))
    case _ => Empty
  }

  @annotation.tailrec
  def drop(n: Int): Stream[A] = this match {
    case Empty => Empty
    case Cons(_, t) if n > 0 => t().drop(n - 1)
    case Cons(_, _) if n <= 0 => this
  }

  //5.3 Write the function takeWhile for returning all starting elements of a Stream that match the given predicate
  def takeWhile(p: A => Boolean): Stream[A] = ???

  def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
    case Cons(h, t) => f(h(), t().foldRight(z)(f))
    case _ => z
  }

}