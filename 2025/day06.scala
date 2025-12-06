package `2025`.day06

import scala.util.matching.Regex
import scala.collection.immutable.NumericRange

import reader.*
import scala.annotation.tailrec
import scala.util.Try
import java.nio.file.Files
import java.io.File
import scala.collection.immutable.Range.Inclusive

@main def part1() =
  val lines = readFromFile(new File("2025/day06.input")).get

  val operations = {
    val operationStrings = lines.last
    operationStrings.split("\\s+").map(_.head).map(parseOperation)
  }

  val numbers = {
    val numberLines = lines.dropRight(1)
    val parsedNumbers =
      numberLines.map(_.trim).map(_.split("\\s+").map(_.toLong))
    parsedNumbers.transpose
  }

  val results = operations.zip(numbers).map((op, numbers) => numbers.reduce(op))

  println(results.sum)

@main def part2() =
  val lines = readFromFile(new File("2025/day06.input")).get

  val operationWithNumberStartAndEndIdx = {
    val operationStrings = lines.last

    val operationWithNumberStartIdx = operationStrings.zipWithIndex
      .filter((char, idx) => !char.isSpaceChar)
      .map((char, idx) => (parseOperation(char), idx))

    operationWithNumberStartIdx
      .zip(operationWithNumberStartIdx.tail)
      .map((a, b) => (a._1, a._2, b._2 - 1))
      :+ (
        operationWithNumberStartIdx.last._1,
        operationWithNumberStartIdx.last._2,
        operationStrings.length
      )
  }

  val numbersAsStrings = {
    val numberLines = lines.dropRight(1)
    val parsedNumbers =
      numberLines.map(line =>
        operationWithNumberStartAndEndIdx.map {
          case (_, startIndex, endIndex) =>
            line.substring(startIndex, endIndex)
        }
      )
    parsedNumbers
  }

  val transposedNumbers = numbersAsStrings.transpose
  val numberLengths = operationWithNumberStartAndEndIdx.map {
    case (_, begin, end) => end - begin - 1
  }
  val rNumbers = transposedNumbers.zip(numberLengths).map {
    case (numbers, maxNumberLength) =>
      partTwoNumber(numbers, maxNumberLength + 1)
  }

  val results = operationWithNumberStartAndEndIdx.zip(rNumbers).map {
    case ((op, _, _), numbers) => {
      val r = numbers.reduce(op)
      println(s"$numbers -> $r")
      r
    }
  }
  println(results.sum)

def sum(a: Long, b: Long) = a + b
def product(a: Long, b: Long) = a * b

def parseOperation(operation: Char): (Long, Long) => Long = operation match
  case '+' => sum
  case '*' => product

def partTwoNumber(numbers: Seq[String], maxNumberLength: Int): Seq[Long] =
  for (idx <- (maxNumberLength - 1).to(0, -1)) yield
    val finalNumber = for {
      number <- numbers if number(idx).isDigit
    } yield number(idx)
    finalNumber.mkString.toLong
  