package com.github.nresni.algrok

import org.scalatest.{FunSpec, Matchers}

class SelSortSpec extends FunSpec with Matchers {

  describe("Smallest of  Array(1, 2, 3)") {
    it("should equals 0") {
      SelSort.findSmallestBasic(
        Array(1, 2, 3)) should be(0)
    }
  }

  describe("Smallest of  Array(2, 1, 3)") {
    it("should equals 1") {
      SelSort.findSmallestBasic(
        Array(2, 1, 3)) should be(1)
    }
  }

  describe("Selection sort of  Array(2, 1, 3)") {
    it("should equals Array(1, 2, 3)") {
      SelSort.selectionSortBasic(Array(2, 1, 3)) should equal(Array(1, 2, 3))
    }
  }

  describe("SmallestRec of  Array(1, 2, 3)") {
    it("should equals 0") {
      SelSort.findSmallestBasicRec(Array(1, 2, 3)) should be(0)
    }
  }

  describe("SmallestRec of  Array(2, 1, 3)") {
    it("should equals 1") {
      SelSort.findSmallestBasicRec(Array(2, 1, 3)) should be(1)
    }
  }

  describe("Selection sort using rec of  Array(2, 1, 3)") {
    it("should equals 0") {
      SelSort.selectionSortBasicUsingRec(Array(2, 1, 3)) should equal(Array(1, 2, 3))
    }
  }
}
