package aoc2021.days

import fastparse._, NoWhitespace._

// Problem Answer Rank
// 1: 2215080 52529
// 2: 1864715580 45968

object Day2 extends AocDay {
  // Actual Logic
  sealed trait Direction
  object Direction {
    case object Forward extends Direction
    case object Down extends Direction
    case object Up extends Direction
  }
  import Direction._

  case class SubCommand(dir: Direction, value: Int)

  trait SubLocation{ def position: Int; def depth: Int }
  case class DirectSubLocation(position: Int, depth: Int) extends SubLocation

  def runDirectCommands(commands: Seq[SubCommand]) : SubLocation = {
    commands.foldLeft(DirectSubLocation(0, 0))((location, command) => command match {
      case SubCommand(Forward, x) => location.copy(position = location.position + x)
      case SubCommand(Down, x) => location.copy(depth = location.depth + x)
      case SubCommand(Up, x) => location.copy(depth = location.depth - x)
    })
  }

  case class AimSubLocation(position: Int, depth: Int, aim: Int) extends SubLocation

  def runAimCommands(commands: Seq[SubCommand]) : SubLocation = {
    commands.foldLeft(AimSubLocation(0, 0, 0))((location, command) => command match {
      case SubCommand(Forward, x) => location.copy(position = location.position + x, depth = location.depth + (x * location.aim))
      case SubCommand(Down, x) => location.copy(aim = location.aim + x)
      case SubCommand(Up, x) => location.copy(aim = location.aim - x)
    })
  }


  // Parsers
  def number[_: P]: P[Int] = P( CharIn("0-9").rep(1).!.map(_.toInt))

  def forward[_: P]: P[Direction] = P( "forward".!.map(_ => Forward) )
  def down[_: P]: P[Direction] = P( "down".!.map(_ => Down) )
  def up[_: P]: P[Direction] = P( "up".!.map(_ => Up) )

  def command[_: P]: P[SubCommand] = P( ((forward | down | up) ~ " " ~ number).map(SubCommand.tupled) )
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

