package com.pogorelovs.aoc

object Day2 {
  def main(args: Array[String]): Unit = {
    val inputLines = readFromConsole(readAllLines)
    val reports = inputLines.map(_.split("\\s").map(_.toLong).toSeq)

    val part1Answer = reports.map(l => isSafe(l)).count(_ == true)
    val part2Answer = reports.map(l => isSafe(l, 1)).count(_ == true)

    println(s"Part1 answer: $part1Answer")
    println(s"Part2 answer: $part2Answer")
  }

  def isSafe(seq: Seq[Long], tolerance: Int = 0): Boolean = {
    def checkSafety(seq: Seq[Long], tolerance: Int = 0, currentLevel: Int = 0): Boolean = {
      def verifyDistance(distance: Long, shouldBePositive: Boolean): Boolean = {
        if (shouldBePositive && distance < 0) return false
        if (!shouldBePositive && distance > 0) return false

        val absDistance = Math.abs(distance)
        absDistance >= 1 && absDistance <= 3
      }

      if (currentLevel > tolerance) return false

      val distanceIndexPair = seq.zip(seq.tail).map(_ - _).zipWithIndex

      val positiveSign = distanceIndexPair.map(_._1).groupBy(_ > 0).map { case (bool, longs) => bool -> longs.length }.withDefault(_ => 0)

      val shouldBePositive = positiveSign(true) > positiveSign(false)

      val distanceChecked = distanceIndexPair.map { case (l, i) => (verifyDistance(l, shouldBePositive), i) }

      val maybeFailed = distanceChecked.find(!_._1)
      if (maybeFailed.isEmpty) return true

      val leftIndexToDrop = maybeFailed.head._2
      val potentialIdxsToDrop = Seq(leftIndexToDrop, leftIndexToDrop + 1)

      val newSequences = potentialIdxsToDrop.map(idxToDrop => seq.patch(idxToDrop, Nil, 1))
      newSequences.exists(checkSafety(_, tolerance, currentLevel + 1))
    }

    checkSafety(seq, tolerance)
  }
}