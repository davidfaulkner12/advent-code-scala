package aoc2021

import aoc2021.days._
import scala.io.Source
import scala.util.{Try,Success,Failure}

object Aoc2021 extends App {

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

  if (args.length != 3) {
    Console.err.println("usage: aoc2021 day problem file")
    sys.exit(1)
  }

  val result = for {
    fileContents <- slurp(args(2))
    day <- loadCompanionObjectForDay(args(0))
    result <- runProblem(day, args(1), fileContents)
  } yield result

  result match {
    // Unix-y, on success we do good things
    case Success(value) => println(value)
    case Failure(error) => println(s"Error: ${error}"); sys.exit(1)
  }
}
