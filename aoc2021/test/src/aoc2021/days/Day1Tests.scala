package aoc2021.days

import utest._
import aoc2021.days.Day1._

import fastparse._, NoWhitespace._

object Day1Tests extends TestSuite {
  val tests = Tests{

   val example = """199
                   |200
                   |208
                   |210
                   |200
                   |207
                   |240
                    |269
                   |260
                   |263""".stripMargin
    test("testExample-problem1") {
      assert("7" == problem1(example))
    }

    test("testExample-problem2") {
      assert("5" == problem2(example))
    }

  }
}
