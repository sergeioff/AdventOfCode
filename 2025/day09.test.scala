package `2025`.day09

import munit.FunSuite

class Tests extends FunSuite:
  test("withComplementaryPoints"):
    assertEquals(
      withComplementaryPoints(Coordinate(5, 9), Coordinate(3, 2)).toSet,
      Set(
        Coordinate(5, 9),
        Coordinate(3, 2),
        Coordinate(5, 2),
        Coordinate(3, 9)
      )
    )

  test("calculateArea - 4 points"):
    assertEquals(calculateArea(Coordinate(5,9), Coordinate(3,2), Coordinate(5,2), Coordinate(3,9)), 24L)
