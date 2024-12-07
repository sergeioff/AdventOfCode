package com.pogorelovs.aoc

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day3Spec extends AnyFlatSpec with Matchers {

  "First part" should "return correct result" in {
    Day3.evaluateExpression("xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))") shouldBe 161
  }

  "Second part" should "return correct result" in {
    Day3.evaluateExpression("xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))") shouldBe 48
  }
}
