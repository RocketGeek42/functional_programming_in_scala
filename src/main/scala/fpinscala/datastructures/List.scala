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
    case Cons(0.0, _) => 0.0 //product of any list starting at 0 is 0
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
  else l match {
      case Nil => Nil
      case Cons(_,t) => drop(t, n-1)
    }

  //3.5 Implement dropWhile, which removes elements from the List prefix as long as they match a predicate
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Cons(h,t) if f(h) => dropWhile(t)(f)
    case _ => l
  }

  //3.6 Implement a function init, that returns a List consisting of all but the last element of a List
  def init[A](l: List[A]): List[A] = ???

}
