package com.github.nresni.algrok

object QuickSort {
  def partition(length: Int, width: Int): Int = {
    def loop(l: Int, w: Int): Int = {
      if (l % w == 0) w
      else if (w % l == 0) l
      else if (l > w) loop(l % w, w)
      else if (l < w) loop (l, w % l)
      else 0
    }
    loop(length, width)
  }

  def loopSum(numbers: List[Int]): Int = {
    var total = 0
    for (i <- numbers) {
      total += i
    }
    total
  }

  def recurSum(numbers: List[Int]): Int = {
    @annotation.tailrec
    def loop(xs: List[Int], acc: Int): Int = xs match {
      case Nil      => acc
      case h :: t   => loop(t, acc + h)
    }
    loop(numbers, 0)
  }

  def sum(numbers: List[Int]): Int = numbers.foldLeft(0){(acc, x) => x + acc}
}
