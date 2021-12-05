package aoc2021.days

import utest._
import aoc2021.days.Day4._

object Day4Tests extends TestSuite {
  val tests = Tests{

    val rawExample = """7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1
                    |
                    |22 13 17 11  0
                    | 8  2 23  4 24
                    |21  9 14 16  7
                    | 6 10  3 18  5
                    | 1 12 20 15 19
                    |
                    | 3 15  0  2 22
                    | 9 18 13 17  5
                    |19  8  7 25 23
                    |20 11 10 24  4
                    |14 21 16 12  6
                    |
                    |14 21 17 24  4
                    |10 16 15  9 19
                    |18  8 23 26 20
                    |22 11 13  6  5
                    | 2  0 12  3  7""".stripMargin

    val Array(rawNumbers, rawBoards @ _*) = rawExample.split("\n\n")

    val numbers = rawNumbers.split(",").toSeq

    val boardArrays = rawBoards.map(_.trim.split("\\s+"))

    test("testCreateBoardAndMark") {
      val b = createBoard(boardArrays(0))

      val marked_b = b.mark("7").mark("4")

      marked_b.marked.zipWithIndex.foreach{ case (elem: Boolean, idx: Int) => {
        if (idx == 8 || idx == 14) assert(elem)
        else assert(!elem)
      }}
    }

    test("testRowWinnerFour") {
      assert(Winner.rowWinner(Vector(true, true, false, false)))
      assert(Winner.rowWinner(Vector(false, false, true, true)))
      assert(!Winner.rowWinner(Vector(false, false, true, false)))
      assert(!Winner.rowWinner(Vector(true, false, true, false)))
    }

    test("testColWinnerFour") {
      assert(Winner.colWinner(Vector(true, false, true, false)))
      assert(Winner.colWinner(Vector(false, true, false, true)))
      assert(!Winner.colWinner(Vector(true, true, false, false)))
      assert(!Winner.colWinner(Vector(true, false, false, true)))
    }

    test("SingleBoard") {
      val b = createBoard(boardArrays(0))

      val (winner, n) = playGame(Seq(b), Seq("22", "13", "17", "11", "0"))

      assert("0" == n)

      assert(winner.values("22") == 0)
      assert(winner.values("13") == 1)
      assert(winner.values("17") == 2)
    }

    test("exampleGame") {
      val bs = boardArrays.map(createBoard(_))

      val (winner, n) = playGame(bs, numbers)

      assert("24" == n)

      assert(winner.values("14") == 0)
      assert(winner.values("21") == 1)
      assert(winner.values("17") == 2)

    }

    test("example") {
      assert("4512" == problem1(rawExample))
    }

    test("unplaySingleBoard") {
      val b = createBoard(boardArrays(0))

      val (winner, n) = playGame(Seq(b), Seq("22", "13", "17", "11", "0"))

      assert("0" == n)

      assert(winner.values("22") == 0)
      assert(winner.values("13") == 1)
    }

    test("unplayExampleGame") {
      val bs = boardArrays.map(createBoard(_))

      val (winner, n) = unplayGame(bs, numbers)

      assert(winner.values("3") == 0)
      assert(winner.values("15") == 1)
    }

    test("example-prob2") {
      assert("1924" == problem2(rawExample))
    }
  }
}

