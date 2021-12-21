package aoc2021.days

import utest._
import aoc2021.days.Day7._

object Day7Tests extends TestSuite {
  val tests = Tests{

    val rawExample = "16,1,2,0,4,2,7,1,2,14"

    test("parser") {
      assert(Seq(16, 1) == parse("16,1"))
    }

    test("example") {
      assert("37" == problem1(rawExample))
      assert("168" == problem2(rawExample))
    }
  }
}
