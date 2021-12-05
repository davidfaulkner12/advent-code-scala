package aoc2021.days

import fastparse._, NoWhitespace._

// Problem Answer Rank
// 1: 4421 52529
// 2: 18674 45968

object Day5 extends AocDay {

  case class LineSegment(x1: Int, y1: Int, x2: Int, y2: Int)

  def absRange(a: Int, b: Int) = if (a < b) a to b else a to b by -1

  def generateLineSequence(l: LineSegment, includeDiag: Boolean): Seq[(Int, Int)] = {
    if (l.x1 == l.x2) {
      absRange(l.y1, l.y2).map((l.x2, _))
    } else if (l.y1 == l.y2) {
      absRange(l.x1, l.x2).map((_, l.y1))
    } else if (includeDiag) {
      absRange(l.x1, l.x2)zip(absRange(l.y1, l.y2))
    } else Seq()
  }

  def countMultiplePointsStraightSegments(ls: Seq[LineSegment], includeDiags: Boolean) : Int =
    ls
      .flatMap(generateLineSequence(_, includeDiags))
      .groupBy(identity)
      .filter {
        case(_, times) => times.size > 1
      }.size

  // Parsers
  def number[_: P]: P[Int] = P( CharIn("0-9").rep(1).!.map(_.toInt))
  def lineSegment[_: P]: P[LineSegment] = P( (number ~ "," ~ number ~ " -> " ~ number ~ "," ~ number).map(LineSegment.tupled) )
  def lineSegments[_: P]: P[Seq[LineSegment]] = P(Start ~ lineSegment.rep(sep="\n") ~ "\n".rep(0) ~ End)

  def problem(data: String, includeDiags: Boolean): String = parse(data, lineSegments(_)) match {
      case Parsed.Success(lines, _) => {
        countMultiplePointsStraightSegments(lines, includeDiags).toString
      }
      case Parsed.Failure(_, _, extra) => extra.trace().longAggregateMsg
  }

  def problem1(data: String): String = problem(data, false)

  def problem2(data: String): String = problem(data, true)

}
