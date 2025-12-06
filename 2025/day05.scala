package `2025`.day05

import scala.util.matching.Regex
import scala.collection.immutable.NumericRange

import reader.*
import scala.annotation.tailrec
import scala.util.Try
import java.nio.file.Files
import java.io.File
import scala.collection.immutable.Range.Inclusive

@main def part1() =
  val lines = readFromFile(new File("2025/day05.input")).get
  val splitIndex = lines.indexOf("")
  val (rangeLines, idLines) = {
    val (rangeLines, tempIdLines) = lines.splitAt(splitIndex)
    (rangeLines, tempIdLines.tail)
  }

  val freshRanges = rangeLines.map(parseRange)

  val result = idLines
    .map(_.toLong)
    .filter(idToTest => freshRanges.exists(_.isInRange(idToTest)))
    .map(_ => 1L)
    .sum
  println(result)

@main def part2() =
  val lines = readFromFile(new File("2025/day05.input")).get
  val splitIndex = lines.indexOf("")
  val rangeLines = lines.take(splitIndex)

  val freshRanges = rangeLines.map(parseRange)

  val compactedRanges = compactRanges(freshRanges)

  println(compactedRanges)

  val sum = compactedRanges.map(_.numberOfElements).sum

  println(sum)

case class Range(begin: Long, finish: Long):
  def isInRange(n: Long): Boolean =
    n >= begin && n <= finish

  def numberOfElements: Long = finish - begin + 1

def parseRange(rangeLine: String): Range =
  val parser = """(\d+)-(\d+)""".r
  rangeLine match
    case parser(startRange, endRange) =>
      Range(startRange.toLong, endRange.toLong)

@tailrec
def compactRanges(ranges: Seq[Range]): Seq[Range] = // TODO: should be improved
  @tailrec
  def compactSortedRanges(
      sortedRanges: Seq[Range],
      acc: Seq[Range] = Seq.empty
  ): Seq[Range] =
    if sortedRanges.isEmpty then return acc
    if sortedRanges.size == 1 then return acc :+ sortedRanges.head

    val a = sortedRanges.head
    val b = sortedRanges(1)

    if b.begin.isBetween(a.begin, a.finish)
    then
      compactSortedRanges(
        sortedRanges.drop(2),
        acc :+ Range(
          a.begin,
          Math.max(a.finish, b.finish)
        )
      )
    else compactSortedRanges(sortedRanges.tail, acc :+ a)

  val r1 = compactSortedRanges(ranges.sortBy(_.begin))
  val r2 = compactSortedRanges(r1.sortBy(_.begin))

  if (r1.size == r2.size) return r2
  else compactRanges(r2)

extension (n: Long) def isBetween(a: Long, b: Long): Boolean = n >= a && n <= b
