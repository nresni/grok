package com.github.nresni.fungrok.col

trait AList {
  def head: Int
  def tail: AList
  def isEmpty: Boolean

  def ::(n: Int): AList = SimpleList(n, this)

  def append(n: Int): AList =
    if(isEmpty) SimpleList(n)
    else head :: tail.append(n)

  def concat(ns: AList): AList =
    if(isEmpty) ns
    else head :: tail.concat(ns)

  def concat2(ns: AList): AList = {
    def loop(xs: AList, acc: AList): AList = {
      if(xs.isEmpty) acc
      else loop(xs.tail, xs.head :: acc)
    }
    loop(this, ns)
  }

  def reverse: AList = {
    @annotation.tailrec
    def loop(ns: AList, acc: AList): AList = {
      if (ns.isEmpty) acc
      else loop(ns.tail, ns.head :: acc)
    }
    loop(this, Empty)
  }

  def reverseMatch: AList = {
    @annotation.tailrec
    def loop(ns: AList, acc: AList): AList = ns match {
      case Empty                  => acc
      case SimpleList(head, tail) => loop(tail, head :: acc)
    }
    loop(this, Empty)
  }

  def positionOf(n: Int): Int = {
    @annotation.tailrec
    def loop(ns: AList, pos: Int): Int = {
      if (ns.isEmpty) -1
      else if (ns.head == n) pos
      else loop(ns.tail, pos + 1)
    }
    loop(this, 0)
  }

  def positionOfMatch(n: Int): Int = {
    @annotation.tailrec
    def loop(ns: AList, pos: Int): Int = ns match {
      case Empty                            => -1
      case SimpleList(head, _) if head == n => pos
      case SimpleList(_, tail)              => loop(tail, pos + 1)
    }
    loop(this, 0)
  }

  def valueAt(n: Int): Int = {
    @annotation.tailrec
    def loop(ns: AList, current: Int): Int = {
      if (ns.isEmpty) -1
      else if (current == n) ns.head
      else loop(ns.tail, current + 1)
    }
    loop(this, 0)
  }

  def valueAtMatch(n: Int): Int = {
    @annotation.tailrec
    def loop(ns: AList, current: Int): Int = ns match {
      case Empty                               => -1
      case SimpleList(head, _) if current == n => head
      case SimpleList(_, tail)                 => loop(tail, current + 1)
    }
    loop(this, 0)
  }
}

case object Empty extends AList {
  override def head: Int        = throw new IllegalArgumentException("No head in empty list")
  override def tail: AList      = throw new IllegalArgumentException("No tail in empty list")
  override def isEmpty: Boolean = true
}

case class SimpleList(head: Int, tail: AList = Empty) extends AList {
  override def isEmpty: Boolean = false
  override def ::(n: Int): AList = SimpleList(n, this)
}
