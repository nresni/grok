package com.github.nresni.fungrok.col

import org.scalatest.{FunSpec, Matchers}

class AListSpec extends FunSpec with Matchers {
  describe("AList type") {
    it("create") {
      1 :: 2 :: 3 :: Empty should be (SimpleList(1, SimpleList(2, SimpleList(3, Empty))))
    }

    it("append O(n)") {
      val a = 1 :: Empty
      a.append(2) should be (SimpleList(1, SimpleList(2, Empty)))
    }

    it("concat O(n)") {
      val a = 1 :: Empty
      val b = 2 :: 3 :: Empty
      a.concat(b) should be (SimpleList(1, SimpleList(2, SimpleList(3, Empty))))
    }

    it("concat2 O(n)") {
      val a = 1 :: Empty
      val b = 2 :: 3 :: Empty
      a.concat2(b) should be (SimpleList(1, SimpleList(2, SimpleList(3, Empty))))
    }

    it("reverse O(n)") {
      val a = 1 :: 2 :: 3 :: Empty
      a.reverse should be (SimpleList(3, SimpleList(2, SimpleList(1, Empty))))
      a.reverseMatch should be (SimpleList(3, SimpleList(2, SimpleList(1, Empty))))
    }

    it("positionOf") {
      val a = 1 :: 2 :: 3 :: Empty
      a.positionOf(3) should be (2)
      a.positionOfMatch(2) should be (1)
    }

    it("valueAt") {
      val a = 1 :: 2 :: 3 :: Empty
      a.valueAt(0) should be (1)
      a.valueAtMatch(2) should be (3)
    }
  }
}
