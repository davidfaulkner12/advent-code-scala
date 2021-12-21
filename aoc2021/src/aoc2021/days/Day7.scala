package aoc2021.days

object Day7 extends AocDay {

  def parse(s: String): Seq[Int] = s.trim.split(",").map(_.toInt)

  def linearScore(start: Seq[Int], cur: Int) = start.map(_ - cur).map(_.abs).sum

  def geometricScore(start: Seq[Int], cur: Int) = start.map(_ - cur).map(_.abs).map(x => x * (x + 1) / 2).sum

  def minimumScore(xs: Seq[Int], scoreFn: (Seq[Int], Int) => Int) = (xs.min to xs.max).map(x => (scoreFn(xs, x), x)).minBy{_._1}

  def problem1(data: String) = minimumScore(parse(data), linearScore)._1.toString

  def problem2(data: String) = minimumScore(parse(data), geometricScore)._1.toString
}
