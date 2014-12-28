package com.github.nresni.fungrok.datastructures.producttypes

import org.scalatest.{FunSpec, Matchers}

class ProductTypesSpec extends FunSpec with Matchers {
  describe("TurtleFirst") {
    it("put pen down") {
      val turtle = TurtleFirst(1, 2, 0, false)
      val penDown = turtle.putPenDown
      penDown.penDown should be (true)
    }

    it("pick pen up") {
      val turtle = TurtleFirst(1, 2, 0, true)
      val penPickUp = turtle.pickPenUp
      penPickUp.penDown should be (false)
    }

    it("turn right once") {
      val turtle = TurtleFirst(1, 2, 0, false)
      val right = turtle.turnRight
      right.heading should be (90)
    }

    it("turn right twice or more") {
      val turtle = TurtleFirst(1, 2, 0, false)
      val rightTwice = turtle.turnRight.turnRight
      rightTwice.heading should be (180)
    }

    it("turn left once") {
      val turtle = TurtleFirst(1, 2, 0, false)
      val left = turtle.turnLeft
      left.heading should be (-90)
    }

    it("turn left twice or more") {
      val turtle = TurtleFirst(1, 2, 0, false)
      val leftTwice = turtle.turnLeft.turnLeft
      leftTwice.heading should be (-180)
    }
  }

  describe("Turtle data structured") {
    it("put pen down") {
      val turtle = Turtle(1, 2, Heading(0), Up)
      val penDown = turtle.putPenDown
      penDown.penState should be (Down)
    }

    it("pick pen up") {
      val turtle = Turtle(1, 2, Heading(0), Down)
      val penUp = turtle.pickPenUp
      penUp.penState should be (Up)
    }

    it("turn right once") {
      val turtle = Turtle(1, 2, Heading(0), Down)
      val right = turtle.turnRight
      right.heading should be (Heading(90))
    }

    it("turn right twice or more") {
      val turtle = Turtle(1, 2, Heading(0), Down)
      val rightTwice = turtle.turnRight.turnRight
      rightTwice.heading should be (Heading(180))
    }

    it("turn left once") {
      val turtle = Turtle(1, 2, Heading(0), Down)
      val left = turtle.turnLeft
      left.heading should be (Heading(-90))
    }

    it("turn left twice or more") {
      val turtle = Turtle(1, 2, Heading(0), Down)
      val leftTwice = turtle.turnLeft.turnLeft
      leftTwice.heading should be (Heading(-180))
    }
  }
}
