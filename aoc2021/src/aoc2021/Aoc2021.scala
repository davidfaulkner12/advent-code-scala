package aoc2021

import aoc2021.days._
import scala.io.Source
import scala.util.{Try,Success,Failure}

object Aoc2021 extends App {

  def printUsageAndExit(error: Throwable) : Unit = {
   Console.err.println("usage aoc2021 day problem file")
   Console.err.println(s"Error: ${error}")
   sys.exit(1)
  }

  def slurp(file: String) = Try {
    Source.fromFile(file).mkString
  }

  def loadCompanionObjectForDay(day: String) : Try[AocDay] = Try {
    val c = Class.forName(s"aoc2021.days.Day${day}$$")
    c.getDeclaredField("MODULE$").get().asInstanceOf[AocDay]
  }

  def runProblem(day: AocDay, problem: String, data: String) : Try[String] = Try {
    problem match {
      case "1" => day.problem1(data)
      case "2" => day.problem2(data)
      case _ => throw new IllegalArgumentException("Problem must be '1' or '2'")
    }
  }

  val result = for {
    dayNum <- Try(args(0))
    problemNum <- Try(args(1))
    filename <- Try(args(2))
    fileContents <- slurp(filename)
    day <- loadCompanionObjectForDay(dayNum)
    result <- runProblem(day, problemNum, fileContents)
  } yield result

  result match {
    // Unix-y, on success we just print the output and implicitly exit(0)
    case Success(value) => println(value)
    case Failure(error) => printUsageAndExit(error)
  }
}
