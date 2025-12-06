package `2025`.day02

import munit.FunSuite

class Tests extends FunSuite:
  test("isInvalid"):
    assert(isInvalid("11"))
    assert(isInvalid("22"))
    assert(isInvalid("55"))
    assert(isInvalid("6464"))
    assert(isInvalid("123123"))
    assert(!isInvalid("12"))
    assert(!isInvalid("13"))
    assert(!isInvalid("111"))

  test("isInvalidV2"):
    assert(isInvalidV2("11"))
    assert(isInvalidV2("22"))
    assert(isInvalidV2("99"))
    assert(isInvalidV2("111"))
    assert(isInvalidV2("999"))
    assert(isInvalidV2("1010"))
    assert(isInvalidV2("1188511885"))
    assert(isInvalidV2("2121212121"))
    assert(isInvalidV2("222222"))
    assert(!isInvalidV2("1"))
    assert(!isInvalidV2("10"))
    assert(!isInvalidV2("1001"))

  test("takeFirst"):
    assertEquals("abcdefg".takeFirst(2), ("ab", "cdefg"))
    assertEquals("abcdefg".takeFirst(3), ("abc", "defg"))
    assertEquals("abcdefg".takeFirst(0), ("", "abcdefg"))
    assertEquals("abcdefg".takeFirst(100), ("abcdefg", ""))

  test("getAllParts"):
    assertEquals(
      "2121212121".getAllParts(2),
      Seq("21", "21", "21", "21", "21")
    )
    assertEquals("1188511885".getAllParts(5), Seq("11885", "11885"))
    assertEquals("111".getAllParts(1), Seq("1", "1", "1"))
