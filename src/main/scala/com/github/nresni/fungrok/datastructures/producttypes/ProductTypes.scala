package com.github.nresni.fungrok.datastructures.producttypes

/**
 * Mutable
 */
class TurtleM(xPosition: Int, yPosition: Int, var heading: Int, var penDown: Boolean) {
  def putPenDown(): Unit = penDown = true
  def pickPenUp(): Unit  = penDown = false
  def turnRight(): Unit  = heading = heading + 90
  def turnLeft(): Unit   = heading = heading - 90
}

/**
 * Immutable
 */
case class TurtleFirst(xPosition: Int, yPosition: Int, heading: Int, penDown: Boolean) {
  def putPenDown: TurtleFirst = copy(penDown = true)
  def pickPenUp: TurtleFirst  = copy(penDown = false)
  def turnRight: TurtleFirst  = copy(heading = heading + 90)
  def turnLeft: TurtleFirst   = copy(heading = heading - 90)
}

/**
 * Immutable + Data structures design
 */
sealed abstract class PenState
case object Up extends PenState
case object Down extends PenState

case class Heading(bearing: Int)

case class Turtle(xPosition: Int, yPosition: Int, heading: Heading, penState: PenState) {
  def putPenDown: Turtle = copy(penState = Down)
  def pickPenUp: Turtle  = copy(penState = Up)
  def turnRight: Turtle  = copy(heading = Heading(heading.bearing + 90))
  def turnLeft: Turtle   = copy(heading = heading.copy(heading.bearing - 90))
}


