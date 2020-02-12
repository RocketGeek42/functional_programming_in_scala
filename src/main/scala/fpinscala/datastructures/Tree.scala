package datastructures
sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {
  //3.25 Write a function size that counts the number of nodes (leaves and branches) in a tree.
  def size[A](tree: Tree[A]): Int =
   // fold(tree, 1)( _ + _ )
   // fold(tree, 1) { case (_, b) => b + 1 }
  tree match {
    case Leaf(_) => 1
    case Branch(l, r) => size(l) + size(r)
  }

  //3.26 Write a function maximum that returns the maximum element in a Tree[Int].
  def maximum(tree: Tree[Int]): Int = tree match {
    case Leaf(i) => i
    case Branch(l, r) => maximum(l) max maximum(r)
  }

  //3.27 Write a function depth that returns the maximum path length from the root of a tree to any leaf.
  def depth[A](tree: Tree[A]): Int = tree match {
    case Leaf(_) => 1
    case Branch(l, r) => (depth(l) max depth(r)) + 1
  }

  //3.28 Write a function map, analogous to the method of the same on List, that modifies each element in a tree with a given function.
  def map[A, B](tree: Tree[A])(f: A => B): Tree[B] =
    tree match {
      case Leaf(x) => Leaf(f(x))
      case Branch(l, r) => Branch(map(l)(f), map(r)(f))
    }

  //3.29 Generalize size, maximum, depth and map, writing a new function fold that abstracts over their similarities. Reimplement them in terms of more general function.
  def fold[A, B](t: Tree[A])(f: A => B)(g: (B, B) => B): B =
    t match {
      case Leaf(a) => f(a)
      case Branch(l, r) => g(fold(l)(f)(g), fold(r)(f)(g))
    }
}