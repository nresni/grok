package com.github.nresni.fungrok.datastructures

package object tree {
  def seed(x: Int) = Node(x, Leaf, Leaf)
  
  def insert(x: Int, tree: Tree): Tree = (x, tree) match {
    case (i, Leaf)                    => seed(i)
    case (i, Node(a, l, r)) if a == i => Node(i, l, r)
    case (i, Node(a, l, r)) if a < i  => Node(a, l, insert(i, r))
    case (i, Node(a, l, r)) if a > i  => Node(a, insert(i, l), r)
  }

  def tSize(tree: Tree): Int = tree match {
    case Leaf          => 1
    case Node(_, l, r) => tSize(l) + tSize(r) + 1
  }
}
