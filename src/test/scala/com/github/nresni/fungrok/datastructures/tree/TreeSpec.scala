package com.github.nresni.fungrok.datastructures.tree

import org.scalatest.{FunSpec, Matchers}

class TreeSpec extends FunSpec with Matchers {
  describe("tree sum type data structure") {

    it("seed") {
      seed(5) should be (Node(5, Leaf, Leaf))
    }

    it("insert") {
      val a = seed(5)
      a should be (Node(5, Leaf , Leaf))

      val b = insert(5, a)
      b should be (Node(5, Leaf, Leaf))

      val c = insert(6, b)
      c should be (Node(5, Leaf, Node(6, Leaf, Leaf)))

      val d = insert(3, c)
      d should be (Node(5, Node(3, Leaf, Leaf), Node(6, Leaf, Leaf)))
    }

    it("tSize") {
      val a = seed(4)
      tSize(a) should be (3)

      val b = insert(6, a)
      val c = insert(9, b)
      val d = insert(2, c)

      tSize(d) should be (9)
    }
  }
}
