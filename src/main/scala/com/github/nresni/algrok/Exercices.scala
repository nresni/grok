package com.github.nresni.algrok

object Exercises {

  object Chap4 {
    def newMid(x: Int, y: Int): Int = (1 + x + y) / 2

    def binarySearch(key: Int, xs: List[Int]): Int = {
      def loop(low: Int, mid: Int, high: Int): Int = {
        if (xs(mid) == key) mid
        else if (key < xs(mid)) loop(low, newMid(low, mid), mid)
        else if (key > xs(mid)) loop(mid, newMid(mid, high), high)
        else -1
      }
      xs match {
        case is if is.isEmpty   ⇒ -1
        case is if is.size == 1 ⇒ if (is(0) == key) 0 else -1
        case _                  ⇒ loop(0, xs.size / 2, xs.size - 1)
      }
    }
  }

  def foo(): Unit = {
    val xs: List[Int] = List(1, 2, 3)
  }
}

import Exercises._
object Foo extends App {
  var foo: List[Int] = List()
  var r: Int = Chap4.binarySearch(1, foo)
  assert(r == -1)

  foo = List(1)
  r = Chap4.binarySearch(1, foo)
  assert(r == 0)

  foo = List(1)
  r = Chap4.binarySearch(2, foo)
  assert(r == -1)

  foo = List(1, 2)
  r = Chap4.binarySearch(2, foo)
  assert(r == 1)

  foo = List(1, 2, 3)
  r = Chap4.binarySearch(3, foo)
  assert(r == 2)

  foo = List(1, 2, 5, 6)
  r = Chap4.binarySearch(6, foo)
  assert(r == 3)

  foo = List(1, 2, 5, 6, 7, 8, 9)
  r = Chap4.binarySearch(8, foo)
  assert(r == 5)

  foo = List(1, 2, 5, 6, 7, 8, 9)
  r = Chap4.binarySearch(2, foo)
  assert(r == 1)

  println("OK !")
}
