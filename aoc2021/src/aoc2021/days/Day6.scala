package aoc2021.days

object Day6 extends AocDay {

  def parse(rawData: String) : Map[Int, Long] =
    rawData.trim.split(",").groupBy(identity).map { case (key, value) => (key.toInt, value.size.toLong) }


  // Why not flatmap? flatmap spits out a map and we lose repeated values
  def nextGeneration(data: Map[Int, Long]) = data.map {
    case (0, n) => Seq((6, n),(8,n))
    case (k, n) => Seq((k - 1, n))
  }.flatten.groupBy(_._1).mapValues(_.map(_._2).sum)

  def iterate(n: Int, value: Map[Int, Long]) : Map[Int, Long] = {
    if (n == 0) value
    else iterate(n -1, nextGeneration(value))
  }


  def problem1(data: String) = iterate(80, parse(data)).values.sum.toString

  def problem2(data: String) = iterate(256, parse(data)).values.sum.toString
}
