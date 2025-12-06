package `2025`.day03

import munit.FunSuite

class Tests extends FunSuite:

  test("toIntSeq"):
    assertEquals("987654321111111".toIntSeq, Seq(9,8,7,6,5,4,3,2,1,1,1,1,1,1,1))
    assertEquals("".toIntSeq, Seq.empty)

  test("getLargestNumber"):
    assertEquals(getLargestNumber("987654321111111".toIntSeq), 98)
    assertEquals(getLargestNumber("811111111111119".toIntSeq), 89)
    assertEquals(getLargestNumber("234234234234278".toIntSeq), 78)
    assertEquals(getLargestNumber("818181911112111".toIntSeq), 92)

  test("getLargestNumberV2"):
    assertEquals(getLargestNumberV2("987654321111111".toIntSeq.toList), 987654321111L)
    assertEquals(getLargestNumberV2("811111111111119".toIntSeq.toList), 811111111119L)
    assertEquals(getLargestNumberV2("234234234234278".toIntSeq.toList), 434234234278L)
    assertEquals(getLargestNumberV2("818181911112111".toIntSeq.toList), 888911112111L)
  