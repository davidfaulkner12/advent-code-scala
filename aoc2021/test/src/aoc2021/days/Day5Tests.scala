package aoc2021.days

import utest._
import aoc2021.days.Day5._

import fastparse._, NoWhitespace._

object Day5Tests extends TestSuite {
  val tests = Tests{

    test("testParser") {
      val Parsed.Success(LineSegment(0,9,5,9), _) = parse("0,9 -> 5,9", lineSegment(_))
    }

    val example = """0,9 -> 5,9
                    |8,0 -> 0,8
                    |9,4 -> 3,4
                    |2,2 -> 2,1
                    |7,0 -> 7,4
                    |6,4 -> 2,0
                    |0,9 -> 2,9
                    |3,4 -> 1,4
                    |0,0 -> 8,8
                    |5,5 -> 8,2""".stripMargin

    test("generateLineSegmentsStraight") {
      assert(Seq((0,0)) == generateLineSequence(LineSegment(0,0,0,0), false))
      assert(Seq((3,4), (2,4), (1,4)) == generateLineSequence(LineSegment(3,4,1,4), false))
    }

    test("problem1") {
      assert("5" == problem1(example))
    }

    test("generateLineSegmentsDiag") {
      assert(Seq((9, 7), (8,8), (7,9)) == generateLineSequence(LineSegment(9,7,7,9), true))
    }

    test("problem2") {
      assert("12" == problem2(example))
    }
  }
}
