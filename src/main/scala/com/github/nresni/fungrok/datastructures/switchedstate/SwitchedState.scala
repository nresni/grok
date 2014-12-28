package com.github.nresni.fungrok.datastructures.switchedstate

sealed abstract class SwitchedState {
  def or(that: SwitchedState): SwitchedState = (this, that) match {
    case (Off, Off) => Off
    case (???, _)   => ???
    case (_, ???)   => ???
    case _          => On
  }

  def and(that: SwitchedState): SwitchedState = (this, that) match {
    case (On, On) => On
    case (???, _) => ???
    case (_, ???) => ???
    case _        => Off
  }
}
case object On extends SwitchedState
case object Off extends SwitchedState
case object ??? extends SwitchedState
