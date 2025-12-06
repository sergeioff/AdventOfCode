package `2025`.day05

import munit.FunSuite

class Tests extends FunSuite:

  test("compactRanges"):
    assertEquals(
      compactRanges(
        Seq(
          Range(3, 5),
          Range(10, 14),
          Range(16, 20),
          Range(12, 18)
        )
      ),
      Seq(
        Range(3, 5),
        Range(10, 20)
      )
    )

  test("Range - number of elements"):
    assertEquals(Range(0, 0).numberOfElements, 1L)
    assertEquals(Range(5, 10).numberOfElements, 6L)
    assertEquals(Range(0, 1).numberOfElements, 2L)
