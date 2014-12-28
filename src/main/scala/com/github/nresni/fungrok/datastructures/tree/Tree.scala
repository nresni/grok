package com.github.nresni.fungrok.datastructures.tree

sealed abstract class Tree
case object Leaf extends Tree
case class Node(value: Int, left: Tree, right: Tree) extends Tree
