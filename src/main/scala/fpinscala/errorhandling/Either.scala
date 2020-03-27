package errorhandling

sealed trait Either[+E, +A] {

  //4.6 Implement versions of map, flatMap, orElse, and map2 on Either that operate on the Right value
  def map[B](f: A => B): Either[E, B] = ???

  def flatMap[EE >: E, B](F: A => Either[EE, B]): Either[EE, B] = ???

  def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = ???

  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = ???

  //4.7 Implement sequence and transverse for Either. These should return the first error that's encountered, if there is one.
  def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] = ???
  def transverse[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] = ???

  //4.8 map2 is only able to report one error, what would you need to change in order to report both errors

}
case class Left[+E](value: E) extends Either[E, Nothing]
case class Right[+A](value: A) extends Either[Nothing, A]