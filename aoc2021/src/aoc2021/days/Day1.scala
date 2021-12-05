package aoc2021.days

// Problem Answer Rank
// 1: 1759 52529
// 2: 1805 45968

object Day1 extends AocDay {

  case class CountState(count : Int, current: Int)

  def countInc(xs : Seq[Int]) : Int = xs match {
    case Nil => 0
    case Seq(first, rest @ _*) => rest.foldLeft(CountState(0, first))((state, x) => {
      val nextCount = if (x > state.current) state.count + 1 else state.count
      CountState(nextCount, x)
    }).count
  }

  def countWindow(xs: Seq[Int]) : Int = {
    countInc(xs.sliding(3,1).toSeq.map(_.reduce(_+_)))
  }

  def parseData(data: String) : Seq[Int] = data.split("\\n").map(_.toInt).toSeq

  def problem1(data: String) : String = countInc(parseData(data)).toString

  def problem2(data: String) = countWindow(parseData(data)).toString
}
