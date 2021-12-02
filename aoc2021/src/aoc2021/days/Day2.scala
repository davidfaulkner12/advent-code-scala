package aoc2021.days

import fastparse._, NoWhitespace._

// Problem Answer Rank
// Problem 1: 2215080 52529
// Problem 2: 1864715580 45968

object Day2 extends AocDay {
  // Actual Logic
  object SubCommandDir extends Enumeration {
    type SubCommandDir = Value
    val forward, down, up = Value
  }
  import SubCommandDir._

  case class SubCommand(dir: SubCommandDir, value: Int)

  trait SubLocation{ def position: Int; def depth: Int }
  case class DirectSubLocation(position: Int, depth: Int) extends SubLocation

  def runDirectCommands(commands: Seq[SubCommand]) : SubLocation = {
    commands.foldLeft(DirectSubLocation(0, 0))((location, command) => command match {
      case SubCommand(SubCommandDir.forward, x) => location.copy(position = location.position + x)
      case SubCommand(SubCommandDir.down, x) => location.copy(depth = location.depth + x)
      case SubCommand(SubCommandDir.up, x) => location.copy(depth = location.depth - x)
    })
  }

  case class AimSubLocation(position: Int, depth: Int, aim: Int) extends SubLocation

  def runAimCommands(commands: Seq[SubCommand]) : SubLocation = {
    commands.foldLeft(AimSubLocation(0, 0, 0))((location, command) => command match {
      case SubCommand(SubCommandDir.forward, x) => location.copy(position = location.position + x, depth = location.depth + (x * location.aim))
      case SubCommand(SubCommandDir.down, x) => location.copy(aim = location.aim + x)
      case SubCommand(SubCommandDir.up, x) => location.copy(aim = location.aim - x)
    })
  }


  // Parsers
  def number[_: P]: P[Int] = P( CharIn("0-9").rep(1).!.map(_.toInt))
  def identifier[_: P]: P[String] = P( CharPred(_.isLetterOrDigit).rep(1).!)

  def commandDir[_: P]: P[SubCommandDir] = P( identifier.map(SubCommandDir.withName) )
  def command[_: P]: P[SubCommand] = P( (commandDir ~ " " ~ number).map(SubCommand.tupled) )
  def commandLines[_: P]: P[Seq[SubCommand]] = P(Start ~ command.rep(sep="\n") ~ "\n".rep(0) ~ End)


  def problem(data: String, fn: Seq[SubCommand] => SubLocation) = parse(data, commandLines(_)) match {
      case Parsed.Success(lines, _) => {
        val loc = fn(lines)
        (loc.position * loc.depth).toString
      }
      case Parsed.Failure(_, _, extra) => extra.trace().longAggregateMsg
  }

  // Interface
  def problem1(data: String) = problem(data, runDirectCommands)

  def problem2(data: String) = problem(data, runAimCommands)
}

