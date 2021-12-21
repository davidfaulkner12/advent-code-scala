package aoc2021.days

object Day9 extends AocDay {

  def parse(rawData: String) : (Seq[Int], Int, Int) = {
    val lines = rawData.split("\n+")
    val dim_y = lines.length
    val dim_x = lines(0).length
    (lines.flatMap(_.split("")).map(_.toInt), dim_x, dim_y)
  }

  def coords_around(idx: Int, dim_x: Int, dim_y: Int): Set[Int] = {
    val (x, y) = (idx % dim_x, idx / dim_x)

    val surrounds = Seq((x -1, y), (x + 1, y), (x, y - 1), (x, y + 1)).filter{case (i, j) => (i >= 0 && i < dim_x && j >= 0 && j < dim_y)}
    Set(surrounds.map{ case (i, j) => i + j * dim_x }: _*)
  }

  def isLowPoint(data: Seq[Int], dim_x: Int, dim_y: Int, idx: Int) : Boolean =
    data(idx) < coords_around(idx, dim_x, dim_y).map(data(_)).min

  def lowPoints(data: Seq[Int], dim_x: Int, dim_y: Int): Set[Int] = (0 until data.size).filter(isLowPoint(data, dim_x, dim_y, _)).toSet

  def riskLevel(data: Seq[Int], lowPoints: Set[Int]) : Int = {
    lowPoints.toSeq.map(data(_) + 1).sum
  }

  def problem1(rawData: String): String = {
    val (data, dim_x, dim_y) = parse(rawData)
    val lps = lowPoints(data, dim_x, dim_y)

    riskLevel(data, lps).toString
  }

  def basin(data: Seq[Int], dim_x: Int, dim_y: Int, lp: Int): Set[Int] = {
    def basin_acc(basinSet: Set[Int], newCoordsAround: Set[Int]) : Set[Int] = {
      val nextBasin = basinSet ++ newCoordsAround
      val nextCoordsAround = newCoordsAround.flatMap(coords_around(_, dim_x, dim_y)).filter(x => !nextBasin.contains(x) && data(x) < 9)
      if (nextCoordsAround.size == 0) basinSet ++ newCoordsAround else basin_acc(basinSet ++ newCoordsAround, nextCoordsAround)
    }
    basin_acc(Set(lp), coords_around(lp, dim_x, dim_y).filter(data(_) < 9))
  }

  def problem2(rawData: String): String = {
    val (data, dim_x, dim_y) = parse(rawData)
    val lps = lowPoints(data, dim_x, dim_y)

    val basin_sizes = lps.map(basin(data, dim_x, dim_y, _)).toSeq.map(_.size)

    val sorted_basins = basin_sizes.sorted(Ordering.Int.reverse)

    (sorted_basins(0) * sorted_basins(1) * sorted_basins(2)).toString

  }
}

