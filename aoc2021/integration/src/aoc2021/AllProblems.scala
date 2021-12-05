package aoc2021.days

import utest._
import aoc2021.Aoc2021
import aoc2021.days._

object AllProblems extends TestSuite {

  def runTests(day: AocDay, dayNum: Int, solution1: Any, solution2: Any) = {
    val data = Aoc2021.slurp(s"data/day${dayNum}.txt").get
    assert(solution1.toString == day.problem1(data))
    assert(solution2.toString == day.problem2(data))
  }

  val tests = Tests {
    test("Day0") {
      val data = Aoc2021.slurp("data/day0.txt").get
      assert("2" == Day0.problem1(data))
    }
    test("Day1") {
      runTests(Day1, 1, 1759, 1805)
    }
    test("Day2") {
      runTests(Day2, 2, 2215080, 1864715580)
    }
    test("Day3") {
      runTests(Day3, 3, 2035764, 2817661)
    }
    test("Day4") {
      runTests(Day4, 4, 27027, 36975)
    }
  }
}
