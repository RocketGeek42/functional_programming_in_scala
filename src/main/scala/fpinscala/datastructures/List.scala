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

  def tail[A](list: List[A]): List[A] = list match {
    case Nil => Nil
    case Cons(_,t) => t
  }

  def setHead[A](list: List[A], head: A): List[A] = ???

  def drop[A](1: List[A], n: Int): List[A] = ???

  def dropWhile[A](1: List[A], f: A => Boolean): List[A] = ???

  def init[A](1: List[A]): List[A] = ???

}
