package errorhandling

sealed trait Option[+A]{
  //4.1 Implement all preceding functions on Option
  def map[B](f: A => B): Option[B] = this match { //Apply f if the option is not None
    case Some(a) => Some(f(a))
    case None => None
  }
  def flatMap[B](f: A => Option[B]): Option[B] = { //Apply f, which may fail, to the Option if not None
    map(f) getOrElse None
  }
  def getOrElse[B >: A](default: => B): B = this match { //The B >: A says that the B type parameter must be a supertype of A
    case Some(a) => a
    case None => default
  }
  def orElse[B >: A](ob: Option[B]): Option[B] = { //Don't evaluate ob unless needed
    map(Some(_)) getOrElse ob
  }
  def filter(f: A => Boolean): Option[A] = { //Convert Some to None if the value doesn't satisfy f.
    flatMap((a: A) => if(f(a)) Some(a) else None)
  }
  //4.2 Implement the variance function in terms of flatmap.
  //If the mean of a sequence is m, the variance is the mean of math.pow(x - m, 2) for each element x in the sequence.
  def mean(xs: Seq[Double]): Option[Double] = {
    if(xs.isEmpty) None
    else Some(xs.sum / xs.length)
  }
  def variance(xs: Seq[Double]): Option[Double] = {
    mean(xs).flatMap(m => mean(xs.map(x => math.pow(x-m, 2))))
  }

  //4.3 Write a generic function map2 that combines two Option values using binary function.
  //If either Option value is None, then return that value too.
  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A,B) => C): Option[C] = (a, b) match {
    case (Some(a), Some(b)) => Some(f(a,b))
    case _ => None
  }

  //4.4 Write a function sequence that combines a list of Options into one Option containing a list of all the Some values in original list.
  def sequence[A](a: List[Option[A]]): Option[List[A]] = ???
}
case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

