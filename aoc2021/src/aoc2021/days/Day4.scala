package aoc2021.days

// Problem Answer Rank
// 1: 27027 47361
// 2: 36975 43975

object Day4 extends AocDay {

  object Winner {
    def rowWinner(marked: Vector[Boolean]) = {
      marked
        .grouped(scala.math.sqrt(marked.length).toInt)
        .map(_.forall(identity))
        .exists(identity _)
    }
    def colWinner(marked: Vector[Boolean]) = {
      def size = scala.math.sqrt(marked.length).toInt
      (0 until size).map(i => marked.grouped(size).map(j => j(i)))
        .map(_.forall(identity))
        .exists(identity _)
    }

    def winner(marked: Vector[Boolean]) = {
      rowWinner(marked) || colWinner(marked)
    }
  }

  case class Board(values: Map[String, Int], marked: Vector[Boolean]) {
    def mark(number: String) = values.get(number) match {
      case None => this
      case Some(n) => Board(values, marked.updated(n, true))
    }
  }

  def createBoard(numberArray: Array[String]): Board = {
    val mappedNumbers = for {
      i <- 0 to 4
      j <- 0 to 4
      idx = (i * 5) + j
    } yield (numberArray(idx), idx)

    Board(Map.empty ++ mappedNumbers, Vector.fill(mappedNumbers.length)(false))
  }

  def playGame(boards: Seq[Board], numbers: Seq[String]) : (Board, String) = {
    val Seq(next, rest @ _*) = numbers

    val nextBoards: Seq[Board] = boards.map(_.mark(next))

    nextBoards.filter((b : Board) => Winner.winner(b.marked)) match {
      case Seq(board, _*) => (board, next)
      case _ => playGame(nextBoards, rest)
    }
  }

  def unplayGame(boards: Seq[Board], numbers: Seq[String]) : (Board, String) = {
    val Seq(next, rest @ _*) = numbers

    val nextBoards: Seq[Board] = boards.map(_.mark(next)).filter((b: Board) => !Winner.winner(b.marked))

    if (nextBoards.length == 1) playGame(nextBoards, rest) else unplayGame(nextBoards, rest)
  }


  def scoreBoard(board: Board, n: Int) = {
    val unmarked = for {
      (k, v) <- board.values
      n = k.toInt
      if !board.marked(v)
    } yield n

    unmarked.sum * n
  }

  def problem(rawData: String, gameFn: (Seq[Board], Seq[String]) => (Board, String)) = {
    val Array(rawNumbers, rawBoards @ _*) = rawData.split("\n\n")

    val numbers = rawNumbers.split(",").toSeq

    val bs = rawBoards.map(_.trim.split("\\s+")).map(createBoard(_))

    val (winner, n) = gameFn(bs, numbers)

    scoreBoard(winner, n.toInt).toString
  }

  def problem1(rawData: String) = problem(rawData, playGame)

  def problem2(rawData: String) = problem(rawData, unplayGame)

}
