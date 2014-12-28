package com.github.nresni.fungrok.datastructures

package object switchedstate {
  def toggle(switch: SwitchedState): SwitchedState = switch match {
    case On  => Off
    case Off => On
    case ??? => ???
  }

  def either(a: SwitchedState, b: SwitchedState): SwitchedState = (a, b) match {
    case (Off, Off) => Off
    case (???, _)   => ???
    case (_, ???)   => ???
    case _          => On
  }

  def ALL(a: SwitchedState, b: SwitchedState): SwitchedState = (a, b) match {
    case (On, On) => On
    case (???, _) => ???
    case (_, ???) => ???
    case _        => Off
  }
}
