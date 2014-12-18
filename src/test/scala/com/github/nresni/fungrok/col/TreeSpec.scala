package com.github.nresni.fungrok.col

import org.scalatest.{FunSpec, Matchers}

class TreeSpec extends FunSpec with Matchers {
  describe("Tree") {

    it("structural") {
      val a = BST(1)
      a should be (BST(1, Leaf, Leaf))

      val b = BST(3)
      b should be (BST(3, Leaf, Leaf))

      val c = BST(2, a, b)
      c should be (BST(2,
                          BST(1, Leaf, Leaf),
                          BST(3, Leaf, Leaf)))
      c.value should be(2)
      c.left.value should be(1)
      c.right.value should be(3)

      val d = BST(6, Leaf, BST(9))
      d should be (BST(6, Leaf, BST(9, Leaf, Leaf)))

      val e = BST(4, Leaf, d)
      e should be (BST(4, Leaf, BST(6, Leaf, BST(9, Leaf, Leaf))))
    }

    it("add on empty") {
      val a = Leaf
      a.add(1) should be (BST(1, Leaf, Leaf))
    }

    it("add value greater than actual value") {
      val a = BST(1)
      a.add(3) should be (BST(1, Leaf, BST(3)))
    }

    it("add value less than actual value") {
      val a = BST(2)
      a.add(1) should be (BST(2, BST(1), Leaf))
    }

    it("add recursive value ") {
      val a = BST(3, BST(2), Leaf)
      a.add(1) should be (BST(3, BST(2, BST(1, Leaf), Leaf), Leaf))

      val b = BST(3, BST(1), Leaf)
      b.add(2) should be (BST(3, BST(1, Leaf, BST(2, Leaf, Leaf)), Leaf))
    }

    it("Depth First traversable on empty") {
      val a = Leaf
      a.depthFirst should be (List.empty)
    }

    it("Depth First traversable one elem") {
      val a = BST(1)
      a.depthFirst should be (List(1))
    }

    it("Depth First traversable two or more elems") {
      val a = BST(4,
                    BST(2,
                          BST(1),
                          BST(3)),
                    BST(6, Leaf,
                           BST(9)))
      a.depthFirst should be (List(4, 2, 1, 3, 6, 9))
    }

    it("Depth First TailRec traversable two or more elems") {
      val a = BST(4,
                    BST(2,
                          BST(1),
                          BST(3)),
                    BST(6, Leaf,
                           BST(9)))
      a.depthFirstTailRec should be (List(4, 2, 1, 3, 6, 9))
    }

    it("Max on an empty tree") {
      intercept[IllegalArgumentException] {
        Leaf.max
      }
    }

    it("Max on one element") {
      BST(1).max should be (1)
    }

    it("Max on two or more elements") {
      BST(1, Leaf, BST(2)).max should be (2)
    }

    it("MaxMatch on two or more elements") {
      BST(2, BST(1), BST(4, Leaf, BST(9))).max should be (9)
      BST(2, BST(1), BST(4, Leaf, BST(9))).maxMatch should be (9)
    }

    it("Min on an empty tree") {
      intercept[IllegalArgumentException] {
        Leaf.min
      }
    }

    it("Min on one element") {
      BST(1).min should be (1)
    }

    it("Min on two or more elements") {
      BST(4, BST(3, BST(2), BST(5)), BST(9)).minMatch should be (2)
      BST(4, BST(3, BST(2), BST(5)), BST(9)).minMatch should be (2)
    }

    it("Inverse an empty tree") {
      Leaf.inverse should be (Leaf)
    }

    it("Inverse a tree with one element") {
      BST(1).inverse should be (BST(-1))
    }

    it("Inverse a tree with two or more elements") {
      BST(2, BST(1), Leaf).inverse should be (BST(-2, Leaf, BST(-1)))
      BST(2, BST(1), BST(4, BST(3), Leaf)).inverse should be (BST(-2, BST(-4, Leaf, BST(-3)), BST(-1)))
    }
  }
}
