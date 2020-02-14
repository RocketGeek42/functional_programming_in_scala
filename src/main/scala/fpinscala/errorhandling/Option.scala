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
}
case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

