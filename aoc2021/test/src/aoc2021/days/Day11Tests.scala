package aoc2021.days

import utest._
import aoc2021.days.Day11._

object Day11Tests extends TestSuite {
  val tests = Tests{
    val rawExample = """5483143223
                       |2745854711
                       |5264556173
                       |6141336146
                       |6357385478
                       |4167524645
                       |2176841721
                       |6882881134
                       |4846848554
                       |5283751526""".stripMargin
    test("parsing works") {
      val example = parse(rawExample)
      assert(example(0) == 5)
      assert(example(2) == 8)
    }

    test("surround works") {
      val output0 = coords_around(0, 3)
      assert(Set(0, 1, 3, 4) == output0)
      val output1 = coords_around(1, 3)
      assert(Set(0, 1, 2, 3, 4, 5) == output1)
      val output2 = coords_around(2, 3)
      assert(Set(1, 2, 4, 5) == output2)
      val output3 = coords_around(3, 3)
      assert(Set(0, 1, 3, 4, 6, 7) == output3)
      val output4 = coords_around(4, 3)
      assert(Set(0, 1, 2, 3, 4, 5, 6, 7, 8) == output4)
      val output5 = coords_around(5, 3)
      assert(Set(1, 2, 4, 5, 7, 8) == output5)
      val output6 = coords_around(6, 3)
      assert(Set(3, 4, 6, 7) == output6)
      val output7 = coords_around(7, 3)
      assert(Set(3, 4, 5, 6, 7, 8) == output7)
      val output8 = coords_around(8, 3)
      assert(Set(4, 5, 7, 8) == output8)
    }

    val simple_example = parse("11111\n19991\n19191\n19991\n11111")

    test("rolling flashes") {
      val input = simple_example.map(_ + 1)
      val output = rollingFlashes(input, Set())

      assert(Seq(3, 4, 5, 4, 3, 4, 14, 16, 14, 4, 5, 16, 11, 16, 5, 4, 14, 16, 14, 4, 3, 4, 5, 4, 3) == output)

    }

    test("step") {
      val expectedOutput = parse("34543\n40004\n50005\n40004\n34543")
      val nextStep = step(SquidMap(simple_example))
      assert(nextStep.sum_flashes == 9)
      assert(nextStep.square == expectedOutput)
    }

    test("example-manual") {
      val step1 = step(SquidMap(parse(rawExample)))
      assert(step1.sum_flashes == 0)
      val step2 = step(step1)
      assert(step2.sum_flashes == 35)
      assert(step2.last_num_flashes == 35)
    }

    test("example-iterate") {
      val flashed_num = iterate_until(SquidMap(parse(rawExample)), _.steps >= 2).sum_flashes
      assert(flashed_num == 35)
      val flashed_num_10 = iterate_until(SquidMap(parse(rawExample)),_.steps >= 10).sum_flashes
      assert(flashed_num_10 == 204)
      val flashed_num_100 = iterate_until(SquidMap(parse(rawExample)), _.steps >= 100).sum_flashes
      assert(flashed_num_100 == 1656)
    }

    test("example-problem-1") {
      assert("1656" == problem1(rawExample))
    }

    test("example-problem-2") {
      assert("195" == problem2(rawExample))
    }
  }
}
