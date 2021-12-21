package aoc2021.days

import utest._
import aoc2021.days.Day10._

object Day10Tests extends TestSuite {
  val tests = Tests{

    test("basic_strings") {
      assert(Left(List()) == processLine(List(), "()".toList))
      assert(Right(')') == processLine(List(), ")".toList))
      assert(Left(List('(')) == processLine(List(), "(()".toList))
    }

    val rawExample = """[({(<(())[]>[[{[]{<()<>>
                       |[(()[<>])]({[<{<<[]>>(
                       |{([(<{}[<>[]}>{[]{[(<()>
                       |(((({<>}<{<{<>}{[]{[]{}
                       |[[<[([]))<([[{}[[()]]]
                       |[{[{({}]{}}([{[{{{}}([]
                       |{<[[]]>}<{[{[{[]{()[[[]
                       |[<(<(<(<{}))><([]([]()
                       |<{([([[(<>()){}]>(<<{{
                       |<{([{{}}[<[[[<>{}]]]>[]]""".stripMargin

    val data = rawExample.split("\n+").map(_.toList)

    test("givenExamplesFilter") {
      val corrupted = data.map(processLine(List(), _)).filter{
        case Right(_) => true
        case _ => false
      }
      assert(5 == corrupted.size)
    }

    test("problem1_example") {
      assert("26397" == problem1(rawExample))
    }

    test("basic_unwind") {
      assert(List(')') == unwindStack(List('('), List()))
      val Left(l) = processLine(List(), "[({(<(())[]>[[{[]{<()<>>".toList)
      assert("}}]])})]".toList == unwindStack(l, List()))
    }

    test("scoreCompletion") {
      assert(288957 == score("}}]])})]".toList, 0))
    }

    test("scoreAll") {
      val scoreAll = data.map(processLine(List(), _)).map{
        case Right(_) => 0
        case Left(l) => score(unwindStack(l, List()), 0)
      }.filter(_ != 0).toSeq
      assert(Seq(288957, 5566, 1480781, 995444, 294) == scoreAll)
    }

    test("problem2_example") {
      assert("288957" == problem2(rawExample))
    }
  }
}


