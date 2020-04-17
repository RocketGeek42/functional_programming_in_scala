sealed trait Stream[+A] {

  object Stream {
    def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
      lazy val head = hd
      lazy val tail = tl
      Cons(() => head, () => tail)
    }
    def empty[A]: Stream[A] = Empty

    def apply[A](as: A*): Stream[A] = if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))


    //5.1 Write a function to convert a Stream to a List
    def toList: List[A] = {
      @annotation.tailrec
      def go(s: Stream[A], acc: List[A]): List[A] = s match {
        case Cons(h, t) => go(t(), h() :: acc)
        case _ => acc
      }

      go(this, List()).reverse
    }

    //5.2 Write the function take(n) for returning the first n elements of a Stream, and drop(n) for skipping the first n elements of a Stream.
    def take(n: Int): Stream[A] = this match {
      case empty => empty
      case Cons(h, t) if n > 0 => Cons(h, () => t().take(n - 1))
      case _ => Empty
    }

    def drop(n: Int): Stream[A] = this match {
      case Cons(_, t) if n > 0 => t().drop(n - 1)
      case _ => this
    }

    //5.3 Write the function takeWhile for returning all starting elements of a Stream that match the given predicate. and 5.5 Implement takeWhile using foldRight
    def takeWhile(p: A => Boolean): Stream[A] = this match {
      case Cons(h, t) => if n > 0 cons(h(), t().take(n - 1)) else empty
      case _ => empty
    }

    def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
      case Cons(h, t) => f(h(), t().foldRight(z)(f))
      case _ => z
    }

    def takeWhileFoldRight(p: A => Boolean): Stream[A] =
      foldRight(Stream.empty[A]) { (a, b) =>
        if(p(a)) Stream.cons(a, b) else Empty
      }

    //5.4 Implement forAll, which checks that all elements in the Stream match a given predicate.  Your implementation should terminate the traversal as soon as it encounters a nonmatching value.
    def forAll(p: A => Boolean): Boolean = this match {
      case Cons(h, t) => if(p(h())) t().forAll(p) else false
      case _ => true
    }

    //5.6 Implement headOption using foldRight.
    def headOptionFoldRight: Option[A] =
      foldRight[Option[A]](None)((a, _) => Some(a))

    //5.7 Implement map, filter, append, and flatMap using foldRight. The append method should be non-strict in its argument
    def map[B](f: A => B): Stream[B] =
      foldRight(Stream.empty[B]) { (a, bs) =>
        Stream.cons(f(a), bs)
      }

    def filter(pred: A => Boolean): Stream[A] =
      foldRight(Stream.empty[A]) { (a, b) =>
        if (prod(a)) Stream.cons(a, b) else b
      }

    def append[AA >: A](as: Stream[AA]): Stream[AA] = {
      foldRight(a)((h, t) => cons(h, t))
    }

    def flatMap[B](f: A => Stream[B]): Stream[B] = {
      foldRight(empty[B])((h, t) => f(h) append t)
    }

    //5.8 Generalize ones slightly to the function constant, which returns an infinite Stream of a given value
    def constant[A](a: A): Stream[A] = Stream.cons(a, constant(a))

    //5.9 Write a function that generates an infinite stream of integers, starting from n, then n + 1, n+ 2 and so on.
    def from(n: Int): Stream[Int] = Streamcons(n, from(n + 1))

    //5.10 Write a function fib that generates the infinite stream of Fibonacci numbers: 0, 1, 1, 2, 3, 5, 8, and so on.
    val fibs: Stream[Int] = {
      def go(f0: Int, f1: Int): Stream[Int] =
        cons(f0, go(f1, f0 + f1))
      go(0, 1)
    }

    //5.11 Write a more general stream-building function called unfold.  It takes an initial state, and a function for producing both the next state and the next value in the generated stream
    def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] =
      f(z) match {
        case Some((h,s)) => cons(h, unfold(s)(f))
        case None => empty
      }

    //5.12 Write fibs, from, constant and ones in terms of unfold.
    def fibsViaUnfold: Stream[Int] = unfold((0,1)){ case (f0, f1) => Some((f0,(f1, f0+f1)))}
    def fromViaUnFold(n: Int): Stream[Int] = unfold(n)(n => Some((n,n+1)))
    def constantViaUnFold[A](a: A): Stream[A] = unfold(a)(_ => Some((a,a)))
    def onesViaUnfold: Stream[Int] = unfold(1)(_ => Some((1,1)))

    //5.13 use unfold to implement map, take, takeWhile, zipWith and zipAll.  The zipAll function should continue the traversal as long as either stream has more elements.
    def mapUnfold[B](f: A => B): Stream[B] =
      unfold(this) {
        case Cons(h,t) => Some((f(h()), t()))
        case _ => None
    }
    def takeViaUnfold(n: Int):Stream[A] =
      unfold((this, n)) {
        case (Cons(h,t), 1) => Some((h(), (empty, 0)))
        case (Cons(h,t), n) if n > 1 => Some((h(), (t(), n-1)))
        case _ => None
      }
    def takeWhileUnfold(p: A => Boolean): Stream[A] =
      Stream.unfold(this) {
        case Cons(h, t) =>
          val appliedPredicate = p(h())
          val output = h()
          val nextState = t()

          if (appliedPredicate) Some((output, nextState))else None
      }
    def zipWith[B, C](s2: Stream[B])(f: (A, B) => C): Stream[C] =
      unfold((this, s2)) {
        case (Cons(h1, t1), Cons(h2, t2)) => Some((f(h1(), h2()), (t1(), t2())))
        case _ => None
      }
    def zipAll[B](s2: Stream[B]): Stream[(Option[A], Option[B])] =
      unfold((this, s2)) {
        case (Cons(h1, t1), Cons(h2, t2)) => Some(((Some(h1())), (Some(h2())), ((t1(), t2()))))
        case (Cons(h1, t1), Empty) => Some(((Some(h1())), None), ((t1(), empty)))
        case (Empty, Cons(h2, t2)) => Some(((None, (Some(h2())))), ((empty, t2())))
        case _ => None
      }
    //5.14 Implement startsWith using functions you've written.  It should check if one Stream is a prefix of another.
    def startsWith[A](s: Stream[A]): Boolean = ???

    //5.15 Implement tails using unfold.
    def tails: Stream[Stream[A]] = ???

    //5.16 Generalize tails to the function scanRight, which is like a foldRight that returns the stream of the intermediate results.


  }

}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]