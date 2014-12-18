package com.github.nresni.fungrok.col

abstract class Tree {
  def value: Int
  def left: Tree
  def right: Tree
  def isEmpty: Boolean

  def add(n: Int): Tree = this match {
    case Leaf                                 => BST(n)
    case BST(value, left, right) if n > value => BST(value, left, right.add(n))
    case BST(value, left, right) if n < value => BST(value, left.add(n), right)
    case _                                    => this
  }

  def depthFirst: List[Int] = {
    def loop(list: List[Tree]): List[Int] = {
      if (list.isEmpty) Nil
      else if (list.head.isEmpty) loop(list.tail)
      else list.head.value :: loop(list.head.left ::
                                   list.head.right ::
                                   list.tail)
    }
    loop(List(this))
  }

  def depthFirstTailRec: List[Int] = {
    def loop(list: List[Tree], acc: List[Int]): List[Int] = list match {
      case Nil                 => acc
      case h :: t if h.isEmpty => loop(t, acc)
      case h :: t              => loop(h.left :: h.right :: t, acc :+ h.value )
    }
    loop(List(this), List())
  }

  def max: Int = {
    def loop(t: Tree, max: Int): Int = {
      if (t.isEmpty) max
      else loop(t.right, t.value)
    }
    if (isEmpty) throw new IllegalArgumentException("Empty tree")
    else loop(right, value)
  }

  def maxMatch: Int = {
    def loop(t: Tree, max: Int): Int = t match {
      case Leaf                 => max
      case BST(value, _, right) => loop(right, value)
    }
    if (isEmpty) throw new IllegalArgumentException("Empty tree")
    else loop(right, value)
  }

  def min: Int = {
    def loop(t: Tree, max: Int): Int = {
      if (t.isEmpty) max
      else loop(t.left, t.value)
    }
    if (isEmpty) throw new IllegalArgumentException("Empty tree")
    else loop(left, value)
  }

  def minMatch: Int = {
    def loop(t: Tree, max: Int): Int = t match {
      case Leaf                => max
      case BST(value, left, _) => loop(left, value)
    }
    if (isEmpty) throw new IllegalArgumentException("Empty tree")
    else loop(left, value)
  }

  def inverse: Tree = {
    if (isEmpty) Leaf
    else BST(-value, right.inverse, left.inverse)
  }
}

case object Leaf extends Tree {
  override def value: Int       = throw new IllegalArgumentException("Value on Leaf")
  override def left: Tree       = throw new IllegalArgumentException("Left on Leaf")
  override def right: Tree      = throw new IllegalArgumentException("Right on Leaf")
  override def isEmpty: Boolean = true
}

case class BST(value: Int, left: Tree = Leaf, right: Tree = Leaf) extends Tree { def isEmpty: Boolean = false }
