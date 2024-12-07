package com.pogorelovs.aoc

object Day3 {
  private val expressionPattern = """mul\((\d{1,3}),(\d{1,3})\)|don't\(\).+?do\(\)""".r

  def main(args: Array[String]): Unit = {
    val input = readFromConsole(readAllLines).mkString("")
    println(s"Answer: ${evaluateExpression(input)}")
  }

  def evaluateExpression(expression: String): Long = getValidPairs(expression).map(_ * _).sum

  def getValidPairs(expression: String): Seq[(Long, Long)] =
    expressionPattern.findAllMatchIn(expression).collect {
      case expressionPattern(el1, el2) if el1 != null && el2 != null => (el1.toLong, el2.toLong)
    }.toSeq
}