package reader
import scala.io.{BufferedSource, Source}
import scala.util.{Failure, Success, Using}
import java.io.File
import scala.util.Try

def readFromConsole[R]: Try[Seq[String]] = {
  val readAllLines = (source: BufferedSource) => source.getLines().takeWhile(!_.matches("")).toSeq
  Using(Source.fromInputStream(System.in))(readAllLines)
}

def readFromFile(file: File): Try[Seq[String]] = {
  Using(Source.fromFile(file)){f => f.getLines().toSeq}
}
