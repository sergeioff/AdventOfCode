package `2025`.day02

import scala.util.matching.Regex
import scala.collection.immutable.NumericRange

import reader.*
import scala.annotation.tailrec

@main def part1() = for {
  lines <- readFromConsole
  input = lines.mkString
} println(getInvalidNumbersFromInput(input, isInvalid).sum)

@main def part2() = for {
  lines <- readFromConsole
  input = lines.mkString
} println(getInvalidNumbersFromInput(input, isInvalidV2).sum)

def getInvalidNumbersFromInput(
    input: String,
    validate: String => Boolean
): Seq[Long] = for {
  range <- input.split(",")
  number <- parseRage(range)
  if (validate(number.toString()))
} yield number

def isInvalid(numberToCheck: String): Boolean =
  val (part1, part2) = numberToCheck.splitAt(numberToCheck.length() / 2)
  part1 == part2

def isInvalidV2(numberToCheck: String): Boolean =
  @tailrec
  def recurInvalid(numberToCheck: String, toTake: Int): Boolean =
    if (toTake == 0) return false
    val parts = numberToCheck.getAllParts(toTake)
    parts.forall(_ == parts.head) || recurInvalid(numberToCheck, toTake - 1)

  recurInvalid(numberToCheck, numberToCheck.length() / 2)

def parseRage(range: String): NumericRange[Long] =
  val rangeMatcher = """(\d+)-(\d+)""".r
  range match
    case rangeMatcher(rangeStart, rangeEnd) =>
      (rangeStart.toLong to rangeEnd.toLong)
    case _ => throw new IllegalArgumentException(s"Can't parse range: $range")

extension (s: String)
  def takeFirst(n: Int): (String, String) = (s.take(n), s.drop(n))

  @tailrec def getAllParts(
      splitBy: Int,
      acc: Seq[String] = Seq.empty
  ): Seq[String] =
    val (part, tail) = s.takeFirst(splitBy)
    if (tail.isEmpty()) acc :+ part
    else tail.getAllParts(splitBy, acc :+ part)
