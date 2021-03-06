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
    test("Day5") {
      runTests(Day5, 5, 4421, 18674)
    }
    test("Day6") {
      runTests(Day6, 6, 388419, 1740449478328L)
    }
    test("Day7") {
      runTests(Day7, 7, 344297, 97164301)
    }
    test("Day9") {
      runTests(Day9, 9, 541, 847504)
    }
    test("Day10") {
      runTests(Day10, 10, 344193, 3241238967L)
    }
    test("Day11") {
      runTests(Day11, 11, 1741, 440)
    }
    test("Day12") {
      runTests(Day12, 12, 5457, 128506)
    }
    test("Day16") {
      runTests(Day16, 16, 996, "96257984154")
    }
  }
}
