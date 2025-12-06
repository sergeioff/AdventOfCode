package `2025`.day01
import scala.util.matching.Regex
import scala.util.chaining.scalaUtilChainingOps
import reader.*

type Operation = (Int, Int) => Int

final case class Turn(operation: Operation, operand: Int)
val sum = (a: Int, b: Int) => a + b
val subtract = (a: Int, b: Int) => a - b

def parseTurn(turn: String): Turn =
  val turnParser = """([RL])(\d+)""".r
  turn match
    case turnParser(operation, number) =>
      val parsedNumber = number.toInt
      operation match
        case "R" => Turn(sum, parsedNumber)
        case "L" => Turn(subtract, parsedNumber)
    case _ => throw new IllegalArgumentException("Can't parse turn")

val leftBorder = 0
val rightBorder = 99
val zeroMatch = 100

final case class Result(result: Int, numberOfZeroClicks: Int)

def rotate(
    startingPoint: Int,
    operation: Operation,
    turnsToDo: Int,
    numberOfZeroClicks: Int = 0
): Result =
  // val Result(newPoint, clicksToAdd) = {
  //   val r = operation(startingPoint, turnsToDo)
  //   if (r < leftBorder) rotate(zeroMatch, sum, r, if (startingPoint != 0) numberOfZeroClicks + 1 else numberOfZeroClicks) else Result(r, numberOfZeroClicks)
  // }
  // val result = newPoint % zeroMatch
  // val numberOfZeroMatches = {
  //   val r = newPoint / zeroMatch
  //   if (startingPoint != result && result == 0 && newPoint != 100) r + 1 else r
  // }
  // Result(result, numberOfZeroMatches + clicksToAdd)
  val operationResult = operation(startingPoint, turnsToDo)
  val newPosition = operationResult % zeroMatch
  val zeroClicks = Math.abs(operationResult / zeroMatch)

  val finalPosition = if (newPosition < leftBorder) zeroMatch + newPosition else newPosition
  val finalZeroClicks = if (startingPoint != 0 && finalPosition == 0 && operationResult != 100) zeroClicks + 1 else zeroClicks

  Result(finalPosition, finalZeroClicks)

final case class State(currentState: Int, numberOfZeroClicks: Int)
@main def part1() =
  val inputTurns = readFromConsole.get // FIXME

  val startingPoint = State(currentState = 50, numberOfZeroClicks = 0)

  val result = inputTurns.foldLeft(startingPoint) { (state, currentTurn) =>
    val parsedTurn = parseTurn(currentTurn)
    val result = rotate(state.currentState, parsedTurn.operation, parsedTurn.operand)
    val newNumberOfLeft =
      if (result.result == 0) state.numberOfZeroClicks + 1
      else state.numberOfZeroClicks
    State(result.result, newNumberOfLeft)
  }
  println(result)

// TODO: should be fixed
@main def part2() =
  val inputTurns = readFromConsole.get // FIXME

  val startingPoint = State(currentState = 50, numberOfZeroClicks = 0)

  val result = inputTurns.foldLeft(startingPoint) { (state, currentTurn) =>
    val parsedTurn = parseTurn(currentTurn)
    // val result = rotate(state.currentState, parsedTurn.operation, parsedTurn.operand)
    val result = calculateWithOverflow(state.currentState, currentTurn.head, parsedTurn.operand)

    println(s"$state + $currentTurn = $result")

    // val newNumberOfLeft =
    //   if (result.result == 0 && state.currentState != 0) state.numberOfZeroClicks + 1
    //   else state.numberOfZeroClicks

    State(result.result, state.numberOfZeroClicks + result.numberOfZeroClicks)
  }
  println(result)


def calculateWithOverflow(currentValue: Int, operation: Char, turnsToDo: Int, numberOfOverflows: Int = 0): Result = 
  operation match
    case 'R' => 
      val result = currentValue + turnsToDo 
      if (result > rightBorder)
        val newValue = 0
        val newTurnsToDo = currentValue + turnsToDo - zeroMatch
        calculateWithOverflow(newValue, 'R', newTurnsToDo, numberOfOverflows + 1)
      else Result(result, numberOfOverflows)
    case 'L' => 
      val result = currentValue - turnsToDo 
      if (result < leftBorder)
        val newValue = 100
        val newTurnsToDo = Math.abs(result)
        calculateWithOverflow(newValue, 'L', newTurnsToDo, numberOfOverflows + 1)
      else Result(result, numberOfOverflows)
    