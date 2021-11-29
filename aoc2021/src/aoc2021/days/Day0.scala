package aoc2021.days

import fastparse._, NoWhitespace._

// Actually an implementation for Day 2 from 2020
// https://adventofcode.com/2020/day/2

object Day0 extends AocDay {
  // Actual Logic
  case class PasswordLine(first : Int, second: Int, c : Char, password: String)

  def testPasswordRange(line: PasswordLine) : Boolean = {
    val charCount = line.password.filter(_ == line.c).length
    line.first <= charCount && charCount <= line.second
  }

  // Parsers
  def number[_: P]: P[Int] = P( CharIn("0-9").rep(1).!.map(_.toInt))
  def char[_: P]: P[Char] = P( CharPred(_.isLetter).!.map(_(0)))
  def identifier[_: P]: P[String] = P( CharPred(_.isLetterOrDigit).rep(1).!)
  def passwordLine[_: P]: P[PasswordLine] = P((number ~ "-" ~ number ~ " " ~ char ~ ": " ~ identifier).map(PasswordLine.tupled))
  def passwordLines[_: P]: P[Seq[PasswordLine]] = P(Start ~ passwordLine.rep(sep="\n") ~ "\n".rep(0) ~ End)

  // Interface
  def problem1(data: String) = parse(data, passwordLines(_)) match {
      case Parsed.Success(lines, _) => lines.filter(testPasswordRange).length.toString
      case Parsed.Failure(_, _, extra) => extra.trace().longAggregateMsg
  }

  def problem2(_x: String) = "<placeholder>"
}

