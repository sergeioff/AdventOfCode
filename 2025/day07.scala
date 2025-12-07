package `2025`.day07

import scala.util.matching.Regex
import scala.collection.immutable.NumericRange

import reader.*
import scala.annotation.tailrec
import scala.util.Try
import java.nio.file.Files
import java.io.File
import scala.collection.immutable.Range.Inclusive

@main def part1() =
  val lines = readFromFile(new File("2025/day07.input.test")).get
  val grid = lines.map(_.split("").map(_.head).toSeq)
  val startPosition = findStartPosition(grid.head)
  val splits = passBeam(grid.tail, Set(startPosition))
  println(splits)

def findStartPosition(firstLine: Seq[Char]) = firstLine.zipWithIndex
  .find { case (element, idx) => element == 'S' }
  .map(_._2)
  .get

def passBeam(grid: Seq[Seq[Char]], beamPositions: Set[Int], splitsAcc: Int = 0): Int =
  val currentLine = grid.head

  val (splitBeamPositions, passBeamPositions) = {
    val groupedBeamPositions = beamPositions
      .groupBy(beamPosition => currentLine(beamPosition) == '^')
      .withDefault(_ => Set.empty[Int])
    (groupedBeamPositions(true), groupedBeamPositions(false))
  }

  val newBeamPositions = getBeamSplitPositions(currentLine, splitBeamPositions) ++ passBeamPositions
  println(newBeamPositions.foldLeft(currentLine)((op, i) => op.patch(i, "|", 1)).mkString)

  if (grid.tail.isEmpty) splitsAcc + splitBeamPositions.size
  else passBeam(grid.tail, newBeamPositions, splitsAcc + splitBeamPositions.size)

def getBeamSplitPositions(line: Seq[Char], startLocations: Set[Int]): Set[Int] =
  for {
    splitPosition <- startLocations
    potentialLeftSplit = splitPosition - 1
    potentialRightSplit = splitPosition + 1
    potentialPosition <- Seq(potentialLeftSplit, potentialRightSplit)
    if potentialPosition >= 0 && potentialPosition <= line.size
    if line(potentialPosition) == '.'
  } yield potentialPosition
 