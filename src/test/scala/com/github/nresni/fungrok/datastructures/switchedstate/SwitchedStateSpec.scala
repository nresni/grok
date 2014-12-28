package com.github.nresni.fungrok.datastructures.switchedstate

import org.scalatest.{FunSpec, Matchers}

class SwitchedStateSpec extends FunSpec with Matchers {
  describe("SwitchedState") {
    it("toggle") {
      toggle(On) should be (Off)
      toggle(Off) should be (On)
      toggle(???) should be (???)
    }

    it("either") {
      either(On, On) should be (On)
      either(On, Off) should be (On)
      either(Off, On) should be (On)
      either(Off, Off) should be (Off)
      either(???, On) should be (???)
      either(On, ???) should be (???)
      either(???, Off) should be (???)
      either(Off, ???) should be (???)
    }

    it("all") {
      ALL(On, On) should be (On)
      ALL(On, Off) should be (Off)
      ALL(Off, On) should be (Off)
      ALL(Off, Off) should be (Off)
      ALL(???, On) should be (???)
      ALL(On, ???) should be (???)
      ALL(???, Off) should be (???)
      ALL(Off, ???) should be (???)
    }

    it("or") {
      On or On should be (On)
      On or Off should be (On)
      Off or On should be (On)
      Off or Off should be (Off)
      ??? or On should be (???)
      On or ??? should be (???)
      ??? or Off should be (???)
      Off or ??? should be (???)
    }

    it("and") {
      On and On should be (On)
      On and Off should be (Off)
      Off and On should be (Off)
      Off and Off should be (Off)
      ??? and On should be (???)
      On and ??? should be (???)
    }

    it("or AND and") {
      On or Off and ??? should be (???)
      On or Off or ??? should be (???)
      On or Off or Off should be (On)
      On and Off and Off should be (Off)
    }
  }
}
