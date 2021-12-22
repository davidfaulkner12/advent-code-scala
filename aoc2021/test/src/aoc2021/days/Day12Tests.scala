package aoc2021.days

import utest._
import aoc2021.days.Day12._

object Day12Tests extends TestSuite {
  val tests = Tests{

    val basicExample = """start-A
                         |start-b
                         |A-b
                         |A-end
                         |b-end""".stripMargin


    test("parser") {
      assert(Map(
        "start" -> Set("A", "b"),
        "A" -> Set("start", "b", "end"),
        "b" -> Set("start", "A", "end"),
        "end" -> Set("A", "b")
      ) == parse(basicExample))
    }

    test("simpleExample") {
      // Just testing termination here
      dfs(parse(basicExample), Set(), SmallVisitOnce(Set()), "start", List("start"))
    }

    val rawExample = """start-A
                       |start-b
                       |A-c
                       |A-b
                       |b-d
                       |A-end
                       |b-end""".stripMargin

    test("realExample") {
      val outputSet = dfs(parse(rawExample), Set(), SmallVisitOnce(Set()), "start", List("start"))
      val expected = Set(
        List("start", "A", "end"),
        List("start", "A", "b", "A", "end"),
        List("start", "A", "c", "A", "end"),
        List("start", "b", "A", "c", "A", "end"),
        List("start", "A", "c", "A", "b", "end"),
        List("start", "A", "b", "A", "c", "A", "end"),
        List("start", "b", "A", "end"),
        List("start", "A", "b", "end"),
        List("start", "A", "c", "A", "b", "A", "end"),
        List("start", "b", "end")
      )

      assert(expected == outputSet)
    }

    test("problem1Example") {
      assert("10" == problem1(rawExample))
    }

    test("problem2Example") {
      val output = problem2(rawExample)
      assert("36" == output)
    }
  }
}

