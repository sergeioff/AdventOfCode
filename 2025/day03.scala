package `2025`.day03

import scala.util.matching.Regex
import scala.collection.immutable.NumericRange

import reader.*
import scala.annotation.tailrec
import scala.util.Try

@main def part1() =
  val largestNumbers = for {
    lines <- readFromConsole
    largestNumbers = for { line <- lines } yield getLargestNumber(line.toIntSeq)
  } println(largestNumbers.sum)

@main def part2() =
  val largestNumbers = for {
    lines <- readFromConsole
    largestNumbers = for { line <- lines } yield {
      val r = getLargestNumberV2(line.toIntSeq.toList)
      println(s"$line -> $r")
      r
    }
  } println(largestNumbers.sum)

def getLargestNumber(numbers: Seq[Int], acc: (Int, Int) = (0, 0)): Int =
  val currentValue = numbers.head
  if (numbers.tail.isEmpty)
    return (acc._1.toString() + Math.max(acc._2, currentValue).toString()).toInt
  else
    val newAcc = currentValue match
      case v if v > acc._1 => (v, 0)
      case v if v > acc._2 => (acc._1, v)
      case _               => acc
    getLargestNumber(numbers.tail, newAcc)

@tailrec
def getLargestNumberV2(
    numbers: List[Int],
    acc: List[Int] = List.fill(12)(0)
): Long =
  val replacementIdxToStart = Math.max(12 - numbers.size, 0)
  val currentValue = numbers.head

  val newAcc = acc.zipWithIndex
    .find((accValue, accIdx) =>
      accIdx >= replacementIdxToStart && currentValue > accValue
    )
    .map((accValue, accIdx) => {
      val replacementSize = acc.size - accIdx
      val replacementAddition = List.fill(Math.max(replacementSize - 1, 0))(0)
      acc.patch(accIdx, currentValue +: replacementAddition, replacementSize)
    })
    .getOrElse(acc)
  if numbers.tail.isEmpty then newAcc.mkString.toLong
  else getLargestNumberV2(numbers.tail, newAcc)

extension (s: String)
  def toIntSeq: Seq[Int] =
    Try { s.split("").map(_.toInt).toSeq }.getOrElse(Seq.empty[Int])

