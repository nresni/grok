package com.github.nresni.algrok

object Recur {

  sealed trait Item
  case class Key() extends Item
  case class Box(items: Option[Seq[Item]] = None) extends Item {
    def getItemsToList: Seq[Item] = items.getOrElse(List.empty)
  }

  /**
   * Find a key - iterative way
   *
   * look_for_key(main_box):
   *   pile = main_box.make_a_pile_to_look_through()
   *   while pile is not empty:
   *     box = pile.grab_a_box()
   *     for item in box:
   *       if item.is_a_box():
   *         pile.append(item)
   *       elif item.is_a_key():
   *         print "found the key"
   */
  def lookForKeyIter(mainBox: Box): Unit = {
    var pile = Iterator(mainBox)
    while(pile.hasNext) {
      val box = pile.next()
      for (item <- box.items.getOrElse(Iterator.empty)) {
        if (isBox(item)) {
          println("Found a box I add it to the pile.")
          pile = pile ++ Iterator(item.asInstanceOf[Box])
        }
        else if (isKey(item)) println("Found the key.")
      }
    }
  }

  private def isBox(item: Item): Boolean = item.isInstanceOf[Box]
  private def isKey(item: Item): Boolean = item.isInstanceOf[Key]

  /**
   * Find a key - Recursive way
   *
   * def look_for_key(box):
   *   for item in box:
   *     if item.is_a_box():
   *       look_for_key(item)
   *     else item.is_a_key():
   *       print "found the key"
   */
  def lookForAKeyRecur(box: Box): Unit = {
    for { item <- box.getItemsToList }
    yield item match {
      case box: Box => lookForAKeyRecur(box)
      case key: Key => println("Found the key")
    }
  }

  /**
   * countdown but it will run forever
   */
  def infiniteCountdown(i: Int): Unit = {
    println(i)
    infiniteCountdown(i-1)
  }

  /**
   * When you write a recursive function the important thing
   * is that you have to tell to stop recursing
   *
   * A recursive function has two parts:
   *   - the base case (when the function does not call itself again)
   *   - the recursive case (when the function calls itself)
   */
  def countdown(i: Int): Unit =  i match {
    case x if x < 0 => println("BANG !")
    case _          => println(i); countdown(i -1)
  }

  /**
   * Call Stack explaining
   *
   * => greet("maggie"):
   *      - Allocates a box of memory for this function
   *      - Allocates a box of memory to stock Maggie into name `variable`
   *    ____________________
   *   |       GREET        \
   *   \--------------------\
   *   |  NAME:  |  MAGGIE  |
   *   ----------------------
   *
   * => "hello, maggie!"
   *
   * => greet2("maggie"):
   *    ____________________
   *   |       GREET2       \ <= current function call (add)
   *   \--------------------\
   *   |  NAME:  |  MAGGIE  |
   *   ----------------------
   *   |       GREET        \ <= partially completed:
   *   |                    |    - call a function from another, paused the caller in a partially completed state
   *   \--------------------\
   *   |  NAME:  |  MAGGIE  |    - all the values of the variable of that function were still stored in memory
   *   ----------------------
   *
   * => "How are you maggie ?"
   *
   *    ____________________
   *   |       GREET2       \ <= remove (pop)
   *   \--------------------\
   *   |  NAME:  |  MAGGIE  |
   *   ----------------------
   *            ..
   *   ----------------------
   *   |       GREET        \ <= back to the caller, here is greet function
   *   \--------------------\
   *   |  NAME:  |  MAGGIE  |
   *   ----------------------
   *
   * => "Getting ready to say bye ..."
   *    ____________________
   *   |         BYE        \ <= current function call (add)
   *   \--------------------\
   *   |       GREET        \
   *   \--------------------|
   *   |  NAME:  |  MAGGIE  |
   *   ----------------------
   *
   * => "Bye !"
   *    ____________________
   *   |         BYE        \ <= remove (pop)
   *   \--------------------\
   *            ..
   *   ----------------------
   *   |       GREET        \ <= back to the caller, here is greet function
   *   \--------------------\
   *   |  NAME:  |  MAGGIE  |
   *   ----------------------
   *
   *            ...
   *
   */
  def greet(name: String): Unit = {
    println("Hello, " + name + " !")
    greet2(name)
    println("Getting ready to say bye ...")
    bye()
  }

  def greet2(name: String): Unit = println("How are you, " + name + " ?")

  def bye(): Unit = println("Ok bye!")

  /**
   * Call stack in recursive function
   *
   * Use case (Factorial): 5! = 5 * 4 * 3 * 2 * 1
   *  - first accumulate                           = fact(3 * fact(3 - 1 * fact(2 - 1)))
   *  - compute (pop the stack, one by one, and make operation) = 1 * 2 * 3
   *
   */
  def fact(x: Int): Int = {
    if (x == 1) 1
    else x * fact(x - 1)
  }

  /**
   * Factorial with tail recursion
   * Basically, the benefice of tail rec is that recursion is translated into loop by the scala compiler and optimize the call stack
   * It translates each call by his result rather than to stack a function before compute the result on pop
   * The only constraint is that the "tail function" must return itself or the result expected
   */
  def factTailRec(x: Int): Int = {
    @annotation.tailrec
    def loop(x: Int, acc: Int): Int = {
      if (x == 1) acc
      else loop(x - 1, acc * x)
    }
    loop(x, 1)
  }
}

