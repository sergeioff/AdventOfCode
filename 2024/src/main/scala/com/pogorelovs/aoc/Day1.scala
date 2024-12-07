package com.pogorelovs.aoc

object Day1 {
  def main(args: Array[String]): Unit = {
    val inputLines = readFromConsole(readAllLines)
    val (leftList, rightList) = parseLists(inputLines)

    println(s"Part1 answer: ${solveTask1(leftList, rightList)}")
    println(s"Part2 answer: ${solveTask2(leftList, rightList)}")
  }

  def solveTask1(leftList: Seq[Long], rightList: Seq[Long]): Long =
    leftList.sorted.zip(rightList.sorted)
      .map { case (id1, id2) => Math.abs(id1 - id2) }
      .sum

  def solveTask2(leftList: Seq[Long], rightList: Seq[Long]): Long =
    val rightMap = rightList.groupBy(identity).map { case (key, values) => key -> values.size }
    leftList.map(id => id * rightMap.getOrElse(id, 0)).sum


  def mergeTuples(tuples: Seq[(Long, Long)]): (Seq[Long], Seq[Long]) = {
    tuples.foldLeft((Seq.empty[Long], Seq.empty[Long]))((acc, tuple) => {
      (acc._1 :+ tuple._1, acc._2 :+ tuple._2)
    })
  }

  def parseLists(lines: Seq[String]): (Seq[Long], Seq[Long]) = {
    val pairs = for {
      line <- lines
      parsed = parseLine(line)
    } yield parsed

    mergeTuples(pairs)
  }

  def parseLine(line: String): (Long, Long) = {
    val lineFormat = "(\\d)\\s+(\\d)".r
    line match
      case lineFormat(el1, el2) => (el1.toLong, el2.toLong)
      case _ => throw new IllegalArgumentException("Wrong line format")
  }
}
