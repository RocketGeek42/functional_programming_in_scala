//Listing 3.1
package datastructures

sealed trait List[+A] //List data type, parameterized on a type, A
case object Nil extends List[Nothing] //A List data constructor representing the empty list
case class Cons[+A](head: A, tail: List[A]) extends List[A] //Another data constructor, representing nonempty lists

object List { //List companion object. Contains functions for creating and working with lists
  def sum(ints: List[Int]): Int = ints match { //A function that uses pattern matching to add up a list of integers
    case Nil => 0 //the sum of the empty list is 0
    case Cons(x,xs) => x + sum(xs) //The sum of a list starting with x is x plus the sum of the rest of the list
  }

  def product(ds: List[Double]): Double = ds match { //pattern matching to return the product of Double elements
    case Nil => 1.0 //product of empty list is 1.0
    //case Cons(0.0, _) => 0.0 //product of any list starting at 0 is 0
    case Cons(x,xs) => x * product(xs) //product of non-empty list is the first element multiplied by the product of the remaining elements
  }

  def apply[A](as: A*): List[A] = //Variadic function syntax
    if (as.isEmpty) Nil
  else Cons(as.head, apply(as.tail: _*))

  //3.2 Implement the function tail for removing the first element of a List
  //function takes a constant time
  def tail[A](list: List[A]): List[A] = list match {
    case Nil => Nil
    case Cons(_,t) => t
  }

  //3.3 Using the same idea, implement the function setHead for replacing the first element of a List with a different value
  def setHead[A](list: List[A], head: A): List[A] = list match {
    case Nil => Nil
    case Cons(_,t) => Cons(head,t)
  }

  //3.4 Generalize tail function to drop, which removes the first n elements from a list
  def drop[A](l: List[A], n: Int): List[A] =
    if (n <= 0) l //if n is less than or equal to 0 just return the list l
  else l match { //match on l - the list given
      case Nil => Nil
      case Cons(_,t) => drop(t, n-1)
    }

  //3.5 Implement dropWhile, which removes elements from the List prefix as long as they match a predicate
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Cons(h,t) if f(h) => dropWhile(t)(f) //curried function
    case _ => l
  }

  //3.6 Implement a function init, that returns a List consisting of all but the last element of a List
  def init[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(_,Nil) => Nil
    case Cons(h,t) => Cons(h,init(t))
  }

  //Listing 3.2 Right folds and simple uses
  def foldRight[A,B](as: List[A], z: B)(f: (A,B) => B): B = as match { //placing f in its own argument group after as and z lets type inference determine the input types to f
    case Nil => z
    case Cons(x, xs) => f(x, foldRight(xs, z)(f))
  }

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x,y) => x + y)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _) //_*_ is more concise notation for (x,y) => x * y

  //3.7 Question - can product, implemented using foldRight, immediately halt the recursion and return 0.0 if it encounters a 0.0?
  def shortcircuitingFoldRight[A, B](as: List[A], z: B) (shortCircuit: A => Boolean)(f: (A, B) => B): B =
    as match {
      case Nil => z
      case Cons(x, _) if shortCircuit(x) => f(x, z)
      case Cons(x, xs) =>
        f(x, shortcircuitingFoldRight(xs, z)(shortCircuit)(f))
    }

  def len[A](as: List[A]): Int =
    foldRight(as, 0)((_, b) => b + 1)

  //3.8 what happens when you pass Nil and Cons to themselves to foldRight
  //You would get back the original list
  //foldRight replaces the 'Nil' constructor of the list with the 'z' argument
  //and replaces the 'Cons' constructor with the given function 'f'
  //foldRight(List(1,2,3), Nil:List[Int])(Cons(_,_))

  //3.9 Compute the length of a list using foldRight

  def length[A](as: List[A]): Int =
    foldRight(as, 0)((_,b) => b + 1)

  //3.10 Write another general list-recursion function, foldLeft, that is tail-recursive
  @annotation.tailrec
  def foldLeft[A,B](as: List[A], z: B)(f: (B,A) => B): B = as match {
    case Nil => z
    case Cons(x,xs) => foldLeft(xs, f(z, x))(f)
  }

  //3.11 Write sum, product and a function to compute the length of a list using foldLeft.
  def sumLeft(as: List[Int]) =
    foldLeft(as, 0)(_ + _)

  def productLeft(as: List[Double]) =
    foldLeft(as, 1.0)(_ * _)

  def lenLeft[A](as: List[A]): Int =
    foldLeft(as, 0)((i, _) => i + 1)

  //3.12 Write a function that returns the reverse of a list. See if you can write it using a fold.
  def reverse[A](as: List[A]): List[A] =
    foldLeft(as, Nil: List[A])((b, a) => Cons(a, b))

  //3.13 Can you write foldLeft in terms of foldRight? Can you write foldRight in terms of foldLeft?
  def foldRightViaFoldLeft[A, B](as: List[A], z: B)(f: (A, B) => B): B =
    foldLeft(reverse(as), z)((b, a) => f(a,b))

  def foldLeftViaFoldRight[A, B](as: List[A], z: B)(f: (B, A) => B): B =
    foldRight(as, (b:B) => b)((a, g) => b => g(f(b,a)))(z)

  //3.14 Implement append in terms of either foldLeft or foldRight
  def appendViaFoldRight[A](as: List[A], z: List[A]): List[A] =
    foldRight(as, z)(Cons(_, _))//as = input list, accumulator (accumulating into base case)

  //3.15 Write a function that concatenates a list of lists into a single list. Its runtime should be linear in the total length of all lists.
  def concatList[A](as: List[List[A]]): List[A] =
    foldRight(as, Nil: List[A])(appendViaFoldRight)

  //3.16 Write a function that transforms a list of integers by adding 1 to each element. Reminder this should be a pure function that returns a new List!
  def plus1(as: List[Int]): List[Int] =
    foldRight(as, Nil: List[Int])((a, b) => Cons(a + 1, b))

  //3.17 Write a function that turns each value in a List[Double] into a String. You can use the expression d.toString to convert some d: Double to a String.
  def doubleToString(as: List[Double]): List[String] =
    foldRight(as, Nil: List[String])((a, b) => Cons(a.toString, b))

  //3.18 Write a function map that generalizes modifying each element in a list while maintaining the structure of the list.
  def map[A,B](as: List[A])(f: A => B): List[B] =
    foldRight(as, Nil: List[B])((a: A, b: List[B]) => Cons(f(a), b))

  //3.19 Write a function filter that removes elements from a list unless they satisfy a given predicate. Use it to remove all odd numbers from a List[Int].
  def filter[A](as: List[A])(f: A => Boolean): List[A] =
    foldRight(as, Nil: List[A])((a,b) => if (f(a)) Cons(a,b) else b)

  //3.20 Write a function flatMap that works like map except that the function given will return a list instead of a single result, and that list should be inserted into the final resulting list.
  def flatMap[A,B](as: List[A])(f: A => List[B]): List[B] =
    concatList(map(as)(f))

  //3.21 Use flatMap to implement filter.
  def filterViaFlatMap[A](as: List[A])(f: A => Boolean): List[A] =
    flatMap(as)(a => if (f(a)) List(a) else Nil)

  //3.22 Write a function that accepts two lists and constructs a new list by adding corresponding elements. For example, List(1,2,3) and List(4,5,6) become List(5,7,9).
  @annotation.tailrec
  def addList(as: List[Int], bt: List[Int]): List[Int] = (as, bt) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(h1,t1), Cons(h2,t2)) => Cons(h1+h2, addList(t1+t2))
  }

  //3.23 Generalize the function you just wrote so that it is not specific to integers or addition. Name your generalized function zipWidth.
  @annotation.tailrec
  def zipWith[A,B,C](as: List[A], bt: List[B])(f: (A,B) => C): List[C] = (as,bt) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(h1,t1), Cons(h2,t2)) => Cons(f(h1,h2), zipWith(t1,t2)(f))
  }

  //3.24 Implement hasSubsequence for checking whether a List contains another List as a subsequence
  @annotation.tailrec
  def startsWith[A](sup: List[A], sub: List[A]): Boolean = (sup, sub) match {
    case (Cons(h1, t1), Cons(h2, t2)) if h1 == h2 => startsWith(t1, t2)
    case (_, Nil) => true
    case _ => false
  }
  @annotation.tailrec
  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = (sup, sub) match {
    case (Cons(h1, t1), Cons(h2, t2)) if h1 == h2 => startsWith(t1, t2)
    case (Cons(_, t1), _) => hasSubsequence(t1,sub)
    case (_, Nil) => true
    case _ => false
  }

}
