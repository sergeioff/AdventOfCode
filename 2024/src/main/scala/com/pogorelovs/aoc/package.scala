package com.pogorelovs

import scala.io.{BufferedSource, Source}
import scala.util.{Failure, Success, Using}

package object aoc {
  def readFromConsole[R](f: BufferedSource => R): R = {
    Using(Source.fromInputStream(System.in))(f) match
      case Failure(exception) => throw exception
      case Success(value) => value
  }

  def readAllLines(source: BufferedSource): Seq[String] = {
    source.getLines().takeWhile(!_.matches("")).toSeq
  }
}
