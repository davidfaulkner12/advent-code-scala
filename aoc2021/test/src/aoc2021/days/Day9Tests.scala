package aoc2021.days

import utest._
import aoc2021.days.Day9._

object Day9Tests extends TestSuite {
  val tests = Tests{
    val rawExample = """2199943210
                       |3987894921
                       |9856789892
                       |8767896789
                       |9899965678""".stripMargin




    test("parser") {
      val (data, 10, 5) = parse(rawExample)
      assert(2 == data(0))
      assert(0 == data(9))
      assert(3 == data(10))
      assert(50 == data.size)
    }

    val (data, _, _) = parse(rawExample)

    test("coords_around") {
      val output0 = coords_around(0, 3, 3)
      assert(Set(1, 3) == output0)
      val output9 = coords_around(9, 10, 5)
      assert(Set(8, 19) == output9)
      val output22 = coords_around(22, 10, 5)
      assert(Set(21, 12, 23, 32) == output22)
    }

    test("low_point") {
      assert(isLowPoint(data, 10, 5, 1))
      assert(isLowPoint(data, 10, 5, 9))
    }

    test("low_points") {
      val output = lowPoints(data, 10, 5)
      assert(Set(1, 9, 22, 46) == output)
    }

    test("problem1_example") {
      assert("15" == problem1(rawExample))
    }

    test("basin") {
      assert(Set(0, 1, 10) == basin(data, 10, 5, 1))
    }

    test("problem2_example") {
      assert("1134" == problem2(rawExample))
    }
  }
}

