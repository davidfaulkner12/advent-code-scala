package aoc2021.days

import scala.util.Either

object Day10 extends AocDay {

  def processLine(stack: List[Char], line: List[Char]): Either[List[Char], Char] = (stack, line) match {
    case (_, Nil) => Left(stack)
    case (_, c :: rest) if Set('(', '[', '{', '<').contains(c) => processLine(c :: stack, rest)
    case ('(' :: popped, ')' :: rest) => processLine(popped, rest)
    case ('{' :: popped, '}' :: rest) => processLine(popped, rest)
    case ('[' :: popped, ']' :: rest) => processLine(popped, rest)
    case ('<' :: popped, '>' :: rest) => processLine(popped, rest)
    case (_, c :: rest) => Right(c)
  }

  def problem1(rawData: String): String = {
    val data = rawData.split("\n+").map(_.toList)

    data.map(processLine(List(), _)).map{
        case Right(')') => 3
        case Right(']') => 57
        case Right('}') => 1197
        case Right('>') => 25137
        case _ => 0
      }.sum.toString

  }

  def unwindStack(stack: List[Char], acc: List[Char]): List[Char] = stack match {
    case Nil => acc.reverse
    case '(' :: popped => unwindStack(popped, ')' :: acc)
    case '{' :: popped => unwindStack(popped, '}' :: acc)
    case '[' :: popped => unwindStack(popped, ']' :: acc)
    case '<' :: popped => unwindStack(popped, '>' :: acc)
  }

  def score(completion: List[Char], acc: Long): Long = completion match {
    case Nil => acc
    case ')' :: rest => score(rest, acc * 5 + 1)
    case ']' :: rest => score(rest, acc * 5 + 2)
    case '}' :: rest => score(rest, acc * 5 + 3)
    case '>' :: rest => score(rest, acc * 5 + 4)
  }

  def problem2(rawData: String): String = {
    val data = rawData.split("\n+").map(_.toList)

    val allOutcomes = data.map(processLine(List(), _)).map{
      case Right(_) => Nil
      case Left(l) => unwindStack(l, List())
    }.filter(_ != Nil)

    val scoreAll = allOutcomes.map(score(_, 0)).sorted

    val index = scoreAll.size / 2

    (scoreAll(index)).toString
  }
}
