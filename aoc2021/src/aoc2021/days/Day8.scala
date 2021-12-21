package aoc2021.days

object Day8 extends AocDay {

  def parseLine(rawData: String): (Seq[String], Seq[String]) = {
    val sides = rawData.split("\\|").map(_.trim.split(" "))
    (sides(0), sides(1))
  }

  def parse(rawData: String): Seq[(Seq[String], Seq[String])] = rawData.split("\n+").map(parseLine(_))

  def outputOnly(data: Seq[(Seq[String], Seq[String])]): Seq[String] = data.flatMap(_._2)

  def problem1(data: String): String = {
    val outputs = outputOnly(parse(data))

    outputs.filter(x => (x.size == 2 || x.size == 3 || x.size == 4 || x.size == 7)).size.toString
  }

  def contradiction(a: Set[Char], b: Set[Char]): Boolean = ((a diff b).size == 0)


  def problem2(data: String): String = ???
}

