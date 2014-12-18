package com.github.nresni.algrok

object SelSort {

  def findSmallestBasic(xs: Array[Int]): Int = {
    var smallest: Int = xs(0)
    var smallestIndex: Int = 0
    for (i <- xs) {
      if(i < smallest){
        smallestIndex = xs.indexOf(i)
        smallest = i
      }
    }
    smallestIndex
  }

  def selectionSortBasic(xs: Array[Int]): Array[Int] = {
    @annotation.tailrec
    def loop(xs: Array[Int], acc: Array[Int]): Array[Int] = {
      if (xs.length == 1) acc :+ xs(0)
      else {
        val smallest = findSmallestBasic(xs)
        val acc1 = acc :+ xs(smallest)
        loop(xs diff acc1, acc1)
      }
    }
    loop(xs, Array())
  }

  def findSmallestBasicRec(xs: Array[Int]): Int = {
    @annotation.tailrec
    def loop(xs: List[Int], smallest: (Int, Int), currentIndex: Int): Int = xs match {
      case Nil                       => smallest._1
      case h :: t if h < smallest._2 => loop(t, (currentIndex, h), currentIndex + 1)
      case _ :: t                    => loop(t, smallest, currentIndex + 1)
    }

    val ls = xs.toList
    loop(ls.tail, (0, ls.head), 1)
  }

  def selectionSortBasicUsingRec(xs: Array[Int]): Array[Int] = {
    @annotation.tailrec
    def loop(xs: Array[Int], acc: Array[Int]): Array[Int] = {
      if (xs.length == 1) acc :+ xs(0)
      else {
        val smallest = findSmallestBasicRec(xs)
        val acc1 = acc :+ xs(smallest)
        loop(xs diff acc1, acc1)
      }
    }
    loop(xs, Array())
  }
}
