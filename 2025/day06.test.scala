package `2025`.day06

import munit.FunSuite

class Tests extends FunSuite:
  test("partTwoNumber"):
    assertEquals(
      partTwoNumber(List("123", " 45", "  6"), 3),
      List[Long](356, 24, 1)
    )
    assertEquals(
      partTwoNumber(List("328", "64 ", "98 "), 3),
      List[Long](8, 248, 369)
    )
    assertEquals(
      partTwoNumber(List(" 51", "387", "215"), 3),
      List[Long](175, 581, 32)
    )
    assertEquals(
      partTwoNumber(List("64 ", "23 ", "314"), 3),
      List[Long](4, 431, 623)
    )
