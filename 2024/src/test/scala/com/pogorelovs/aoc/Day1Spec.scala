package com.pogorelovs.aoc

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day1Spec extends AnyFlatSpec with Matchers {

  "First part" should "return correct result" in {
    val lines = """3   4
                  |4   3
                  |2   5
                  |1   3
                  |3   9
                  |3   3""".stripMargin.split("\n")

    val (l, r) = Day1.parseLists(lines)
    Day1.solveTask1(l, r) shouldBe 11
  }

  "Second part" should "return correct result" in {
    val lines =
      """3   4
        |4   3
        |2   5
        |1   3
        |3   9
        |3   3""".stripMargin.split("\n")

    val (l, r) = Day1.parseLists(lines)
    Day1.solveTask2(l, r) shouldBe 31
  }
}
