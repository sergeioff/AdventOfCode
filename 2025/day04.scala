package `2025`.day04

import scala.util.matching.Regex
import scala.collection.immutable.NumericRange

import reader.*
import scala.annotation.tailrec
import scala.util.Try

type RollsMap = Seq[Seq[Int]]

def transformLine(line: String): Seq[Int] = line.map{
    case '@' => 1
    case '.' => 0
  }

@main def part1() = for {
  lines <- readFromConsole
  map = lines.map(transformLine)
  matchingElements = getIndicesOfAccessibleRolls(map)
} println(matchingElements.size)

@main def part2() = for {
  lines <- readFromConsole
  map = lines.map(transformLine)
} println(getNumberOfAccessibleRollsRecursively(map))

def getIndicesOfAccessibleRolls(map: RollsMap): Seq[(Int, Int)] =
  for {
    lineIdx <- 0 until map.size
    elementIdx <- 0 until map(lineIdx).size
    if map(lineIdx)(elementIdx) == 1
    if map.canAccessElement(lineIdx, elementIdx)
  } yield (lineIdx, elementIdx)

@tailrec
def getNumberOfAccessibleRollsRecursively(
    map: RollsMap,
    replacedRolls: Int = 0
): Int =
  val accessibleRolls = getIndicesOfAccessibleRolls(map)
  if (accessibleRolls.size == 0) return replacedRolls

  val newMap = (0 until map.size).map(lineIdx =>
    (0 until map(lineIdx).size).map(elementIdx =>
      if (accessibleRolls.contains((lineIdx, elementIdx))) 0
      else map(lineIdx)(elementIdx)
    )
  )

  getNumberOfAccessibleRollsRecursively(
    newMap,
    replacedRolls + accessibleRolls.size
  )

extension (matrix: RollsMap)
  def getValueOr0(line: Int, col: Int): Int =
    Try { matrix(line)(col) }.getOrElse(0)

  def canAccessElement(lineIdx: Int, elementIdx: Int): Boolean =
    val upperLeft = matrix.getValueOr0(lineIdx - 1, elementIdx - 1)
    val up = matrix.getValueOr0(lineIdx - 1, elementIdx)
    val upperRight = matrix.getValueOr0(lineIdx - 1, elementIdx + 1)
    val left = matrix.getValueOr0(lineIdx, elementIdx - 1)
    val right = matrix.getValueOr0(lineIdx, elementIdx + 1)
    val bottomLeft = matrix.getValueOr0(lineIdx + 1, elementIdx - 1)
    val bottom = matrix.getValueOr0(lineIdx + 1, elementIdx)
    val bottomRight = matrix.getValueOr0(lineIdx + 1, elementIdx + 1)
    Seq(
      upperLeft,
      up,
      upperRight,
      left,
      right,
      bottomLeft,
      bottom,
      bottomRight
    ).sum < 4
