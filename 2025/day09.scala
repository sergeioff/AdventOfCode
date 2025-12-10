package `2025`.day09

import scala.util.matching.Regex
import scala.collection.immutable.NumericRange

import reader.*
import scala.annotation.tailrec
import scala.util.Try
import java.nio.file.Files
import scala.collection.immutable.Range.Inclusive

import scala.util.Using
import java.io.File

@main def part1() =
  val lines = readFromFile(new File("2025/day09.input")).get
  val coordinates = lines.map(parseLine)
  val pairs = for {
    i <- 0 until coordinates.size
    j <- i + 1 until coordinates.size
  } yield (coordinates(i), coordinates(j))

  val areas = pairs.map { case (a, b) => (a, b, calculateArea(a, b)) }
  println(areas.sortBy(_._3))

def hasEqualCols(a: Coordinate, b: Coordinate): Boolean = a.col == b.col
def hasEqualRows(a: Coordinate, b: Coordinate): Boolean = a.col == b.col

def isCoordinateInBorders(
    coordinate: Coordinate,
    columnBordersByRow: Map[Long, (Long, Long)],
    rowBordersByColumn: Map[Long, (Long, Long)]
): Boolean =
  checkColumnValueInBorders(
    coordinate,
    columnBordersByRow
  ) && checkRowValueInBorders(coordinate, rowBordersByColumn)

def checkColumnValueInBorders(
    coordinate: Coordinate,
    columnBordersByRow: Map[Long, (Long, Long)]
): Boolean =
  columnBordersByRow
    .get(coordinate.row)
    .map((min, max) => coordinate.col >= min && coordinate.col <= max)
    .getOrElse(false)

def checkRowValueInBorders(
    coordinate: Coordinate,
    rowBordersByColumn: Map[Long, (Long, Long)]
): Boolean =
  rowBordersByColumn
    .get(coordinate.col)
    .map((min, max) => coordinate.row >= min && coordinate.row <= max)
    .getOrElse(false)

@main def part2() =
  val lines = readFromFile(new File("2025/day09.input")).get
  val coordinates = lines.map(parseLine)

  val pairs = for {
    i <- 0 until coordinates.size
    j <- i + 1 until coordinates.size
  } yield (coordinates(i), coordinates(j))

  val columnBordersByRow = newGetRowColBorders(coordinates)
  val rowBordersByColumn = newGetColRowBorders(coordinates)

  val rectangleOptions = pairs.map(withComplementaryPoints)

  val rectangleOptionsInBorders = rectangleOptions.filter(coordinateOptions =>
    coordinateOptions.forall(
      isCoordinateInBorders(_, columnBordersByRow, rowBordersByColumn)
    )
  )

  val secondFilteredOptions = rectangleOptionsInBorders
    .filter(coordinates =>
      val rowSorted = coordinates.groupBy(_.row).toSeq.sortBy(_._1)

      val topLeft = rowSorted.head._2.minBy(_.col)
      val topRight = rowSorted.head._2.maxBy(_.col)
      val bottomLeft = rowSorted.last._2.minBy(_.col)
      val bottomRight = rowSorted.last._2.maxBy(_.col)

      val topBorder = for {
        col <- topLeft.col to topRight.col
      } yield Coordinate(topLeft.row, col)
      val rightBorder = for {
        row <- topRight.row to bottomRight.row
      } yield Coordinate(row, topRight.col)
      val leftBorder = for {
        row <- topLeft.row to bottomLeft.row
      } yield Coordinate(row, topLeft.col)
      val bottomBorder = for {
        col <- bottomLeft.col to bottomRight.col
      } yield Coordinate(bottomLeft.row, col)

      topBorder.forall(
        isCoordinateInBorders(_, columnBordersByRow, rowBordersByColumn)
      ) &&
      rightBorder.forall(
        isCoordinateInBorders(_, columnBordersByRow, rowBordersByColumn)
      ) &&
      leftBorder.forall(
        isCoordinateInBorders(_, columnBordersByRow, rowBordersByColumn)
      ) &&
      bottomBorder.forall(
        isCoordinateInBorders(_, columnBordersByRow, rowBordersByColumn)
      )
    )

  val areas = secondFilteredOptions.map {
    case a :: b :: c :: d :: Nil => (calculateArea(a, b, c, d), Seq(a, b, c, d))
    case a :: b :: Nil           => (calculateArea(a, b), Seq(a, b))
  }

  println(areas.maxBy(_._1)._1)

final case class Coordinate(row: Long, col: Long)

def parseLine(line: String): Coordinate =
  val coordinateExtractorPattern = """(\d+),(\d+)""".r
  line match
    case coordinateExtractorPattern(col, row) =>
      Coordinate(row.toLong, col.toLong)

def calculateArea(a: Coordinate, b: Coordinate): Long =
  val rowDiff = Math.abs(a.row - b.row) + 1
  val colDiff = Math.abs(a.col - b.col) + 1
  rowDiff * colDiff

def calculateArea(
    a: Coordinate,
    b: Coordinate,
    c: Coordinate,
    d: Coordinate
): Long =
  val groupedByCol = Seq(a, b, c, d).groupBy(_.col)
  val rowDistance = {
    val p = groupedByCol.head._2.map(_.row)
    assert(p.size == 2)
    Math.abs(p(0) - (p(1))) + 1
  }

  val colDistance =
    Math.abs(groupedByCol.keys.toSeq(0) - groupedByCol.keys.toSeq(1)) + 1
  rowDistance * colDistance

def withComplementaryPoints(pair: (Coordinate, Coordinate)): Seq[Coordinate] =
  val (a, b) = pair
  if (a.row != b.row && a.col != b.col) then
    Seq(a, b, a.copy(col = b.col), b.copy(col = a.col))
  else Seq(a, b)

def newGetRowColBorders(coordinates: Seq[Coordinate]): Map[Long, (Long, Long)] =
  val cols = coordinates
    .groupBy(_.col)
    .map((col, coords) => col -> (coords.minBy(_.row), coords.maxBy(_.row)))
    .toSeq
    .sortBy(_._1)

  val maxRow = coordinates.map(_.row).max
  (0L to maxRow)
    .flatMap(row =>
      val rowCoordinates = cols
        .filter { case (l, (min, max)) => row >= min.row && row <= max.row }
        .map { case (l, (_, _)) => l }

      if (!rowCoordinates.isEmpty)
        Some(row -> (rowCoordinates.min, rowCoordinates.max))
      else None
    )
    .toMap

def newGetColRowBorders(coordinates: Seq[Coordinate]): Map[Long, (Long, Long)] =
  val rows = coordinates
    .groupBy(_.row)
    .map((row, coords) => row -> (coords.minBy(_.col), coords.maxBy(_.col)))
    .toSeq
    .sortBy(_._1)

  val maxCol = coordinates.map(_.col).max
  (0L to maxCol)
    .flatMap(col =>
      val colCoordinates = rows
        .filter { case (l, (min, max)) => col >= min.col && col <= max.col }
        .map { case (l, (_, _)) => l }

      if (!colCoordinates.isEmpty)
        Some(col -> (colCoordinates.min, colCoordinates.max))
      else None
    )
    .toMap

def getColBorders(
    point: Coordinate,
    coordinates: Seq[Coordinate]
): Option[(Long, Long)] =
  val rows = coordinates
    .groupBy(_.row)
    .map((row, coords) => row -> (coords.minBy(_.col), coords.maxBy(_.col)))
  val cols = coordinates
    .groupBy(_.col)
    .map((col, coords) => col -> (coords.minBy(_.row), coords.maxBy(_.row)))

  val columnRowBorders = cols.toSeq
    .sortBy(_._1)
    .filter { case (l, (c1, c2)) =>
      c1.row <= point.row && point.row <= c2.row
    }

  val maxColumnRowBorder =
    columnRowBorders
      .filter { case (l, (c1, c2)) => c1.col >= point.col }
      .map(_._1)
      .maxOption

  val minColumnRowBorder =
    columnRowBorders
      .filter { case (l, (c1, c2)) => c1.col <= point.col }
      .map(_._1)
      .minOption

  minColumnRowBorder.zip(maxColumnRowBorder)

def getRowBorders(
    point: Coordinate,
    coordinates: Seq[Coordinate]
): Option[(Long, Long)] =
  val rows = coordinates
    .groupBy(_.row)
    .map((row, coords) => row -> (coords.minBy(_.col), coords.maxBy(_.col)))

  val rowColumnBorders = rows.toSeq
    .sortBy(_._1)
    .filter { case (l, (c1, c2)) =>
      c1.col <= point.col && point.col <= c2.col
    }

  val maxRowColumnBorder =
    rowColumnBorders
      .filter { case (l, (c1, c2)) => c1.row >= point.row }
      .map(_._1)
      .maxOption

  val minRowColumnBorder =
    rowColumnBorders
      .filter { case (l, (c1, c2)) => c1.row <= point.row }
      .map(_._1)
      .minOption

  minRowColumnBorder.zip(maxRowColumnBorder)
