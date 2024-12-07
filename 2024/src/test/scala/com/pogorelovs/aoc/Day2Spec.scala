package com.pogorelovs.aoc

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day2Spec extends AnyFlatSpec with Matchers {

  "First part" should "return correct result" in {
    Day2.isSafe(Seq(7, 6, 4, 2, 1)) shouldBe true
    Day2.isSafe(Seq(1, 2, 7, 8, 9)) shouldBe false
    Day2.isSafe(Seq(9, 7, 6, 2, 1)) shouldBe false
    Day2.isSafe(Seq(1, 3, 2, 4, 5)) shouldBe false
    Day2.isSafe(Seq(8, 6, 4, 4, 1)) shouldBe false
    Day2.isSafe(Seq(1, 3, 6, 7, 9)) shouldBe true
  }

  "Second part" should "return correct result" in {
    Day2.isSafe(Seq(7, 6, 4, 2, 1), 1) shouldBe true
    Day2.isSafe(Seq(1, 2, 7, 8, 9), 1) shouldBe false
    Day2.isSafe(Seq(9, 7, 6, 2, 1), 1) shouldBe false
    Day2.isSafe(Seq(1, 3, 2, 4, 5), 1) shouldBe true
    Day2.isSafe(Seq(8, 6, 4, 4, 1), 1) shouldBe true
    Day2.isSafe(Seq(1, 3, 6, 7, 9), 1) shouldBe true
  }
}
