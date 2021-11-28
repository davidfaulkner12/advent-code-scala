package aoc2021

import aoc2021.days._
import scala.io.Source

object Aoc2021 extends App {

  def loadCompanionObjectForDay(day: String) = {
    val c = Class.forName(s"aoc2021.days.Day${day}$$")
    c.getDeclaredField("MODULE$").get().asInstanceOf[AocDay]
  }

  if (args.length != 3) {
    Console.err.println("usage: aoc2021 day problem file")
    sys.exit(1)
  }

  val fileContents = Source.fromFile(args(2)).mkString

  val day = loadCompanionObjectForDay(args(0))

  val result = args(1) match {
    case "1" => day.problem1(fileContents)
    case "2" => day.problem2(fileContents)
  }

  println(result)
}
