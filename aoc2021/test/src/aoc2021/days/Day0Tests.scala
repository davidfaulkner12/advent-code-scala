package aoc2021.days

import utest._
import aoc2021.days.Day0._

import fastparse._, NoWhitespace._

object Day0Tests extends TestSuite {
  val tests = Tests{

    test("testParser") {
      val Parsed.Success(PasswordLine(1, 3, 'a', "abcde"), _) = parse("1-3 a: abcde", passwordLine(_))
    }

    test("testExample") {
      val example = """1-3 a: abcde
                      |1-3 b: cdefg
                      |2-9 c: ccccccccc""".stripMargin
      assert("2" == problem1(example))
    }

  }
}
