package aoc2021.days

import utest._
import aoc2021.days.Day6._

object Day6Tests extends TestSuite {
  val tests = Tests{

    val rawExample = "3,4,3,1,2"

    test("parser") {
      assert(Map(2 -> 1, 1 -> 1, 4 -> 1, 3 -> 2) == parse(rawExample))
    }

    test("nextGeneration") {
      assert(Map(1 -> 2, 2 -> 1, 6-> 1, 0 -> 1, 8 ->1) == nextGeneration(Map(2 -> 2, 3 -> 1, 0 -> 1, 1 -> 1)))
    }

    test("iterateOnce") {
      assert(Map(2 -> 2, 3 -> 1, 0 -> 1, 1 -> 1) == iterate(1, parse(rawExample)))
    }

    test("problem1") {
      assert("5934" == problem1(rawExample))
    }

    test("problem2") {
      assert("26984457539" == problem2(rawExample))
    }
  }
}

