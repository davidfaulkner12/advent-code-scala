package aoc2021.days

object Day11 extends AocDay {

  def parse(rawData: String) : Seq[Int] = rawData.split("\n+").flatMap(_.split("")).map(_.toInt)

  def coords_around(idx: Int, dim: Int): Set[Int] = {
    val (x, y) = (idx % dim, idx / dim)

    val surrounds = for {
      i <- (x - 1) to (x + 1)
      j <- (y - 1) to (y + 1)
      if (i >= 0 && i < dim && j >= 0 && j < dim)
    } yield(i, j)

    Set(surrounds.map{ case (i, j) => i + j * dim }: _*)
  }

  def sqrt_int(x: Int) = scala.math.sqrt(x).toInt

  case class SquidMap(square: Seq[Int], sum_flashes: Int = 0, steps: Int = 0, last_num_flashes: Int = 0)

  def increase(square: Seq[Int], flashed_idxs: Seq[Int]) : Seq[Int] = {
    flashed_idxs.foldLeft(square)( (curSquare, idx) => {
      val coords = coords_around(idx, sqrt_int(square.size))
      coords.foldLeft(curSquare)((curSquare2, surrounding_idx) => curSquare2.updated(surrounding_idx, curSquare(surrounding_idx) + 1))
    })
  }

  def rollingFlashes(square: Seq[Int], flashed_idx: Set[Int]) : (Seq[Int]) = {
    val newFlashes = square.zipWithIndex.filter{ case (x, idx) => x > 9 && !flashed_idx.contains(idx) }.map(_._2)

    if (newFlashes.isEmpty) square
    else rollingFlashes(increase(square, newFlashes), flashed_idx ++ newFlashes)
  }

  def step(state: SquidMap): SquidMap = {
    val nextStep = rollingFlashes(state.square.map(_ + 1), Set())
    val flashed_number = nextStep.filter(_ > 9).size

    SquidMap(nextStep.map(x => if (x > 9) 0 else x), state.sum_flashes + flashed_number, state.steps + 1, flashed_number)
  }

  def iterate_until(state: SquidMap, p: SquidMap => Boolean): SquidMap = {
    val nextMap = step(state)
    if (p(nextMap)) nextMap else iterate_until(nextMap, p)
  }


  def problem1(data: String): String = iterate_until(SquidMap(parse(data)), _.steps >= 100).sum_flashes.toString
  def problem2(data: String): String = {
    val start = SquidMap(parse(data))
    iterate_until(start, _.last_num_flashes == start.square.size).steps.toString
  }


}
