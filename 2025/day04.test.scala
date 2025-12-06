package `2025`.day04

import munit.FunSuite

class Tests extends FunSuite:

  test("transformLine"):
    assertEquals(transformLine("@@."), Seq(1, 1, 0))

  test("part2"):
    val input = """|..@@.@@@@.
                   |@@@.@.@.@@
                   |@@@@@.@.@@
                   |@.@@@@..@.
                   |@@.@@@@.@@
                   |.@@@@@@@.@
                   |.@.@.@.@@@
                   |@.@@@.@@@@
                   |.@@@@@@@@.
                   |@.@.@@@.@.""".stripMargin

    val lines = input.split("\n")
    val map = lines.map(transformLine)
    val result = getNumberOfAccessibleRollsRecursively(map)
    assertEquals(result, 43)
