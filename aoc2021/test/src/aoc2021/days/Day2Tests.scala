package aoc2021.days

import utest._
import aoc2021.days.Day2._

import fastparse._, NoWhitespace._

object Day2Tests extends TestSuite {
  val tests = Tests{

    test("testParser") {
      val Parsed.Success(SubCommand(SubCommandDir.forward, 4), _) = parse("forward 4", command(_))
    }

    val example = """forward 5
                    |down 5
                    |forward 8
                    |up 3
                    |down 8
                    |forward 2""".stripMargin

    test("testExampleProb1") {
      assert("150" == problem1(example))
    }

    test("testExampleProb2") {
      assert("900" == problem2(example))
    }

  }
}
