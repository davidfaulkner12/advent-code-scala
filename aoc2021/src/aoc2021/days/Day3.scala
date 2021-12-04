package aoc2021.days

import scala.util.control.Exception._

// Day 3: 2035764
// Day 3: 2817661

object Day3 extends AocDay {

  def problem1(data: String) : String = {
    val s = data.split("\\n")
      .map(_.toArray)
      .transpose.map(_.map(_.getNumericValue))
      .map(xs => xs.sum.toDouble / xs.length)
      .map(_.round.toInt.toString)
      .mkString

    val gamma = Integer.parseInt(s, 2)

    // For an unsigned integer, bitwise NOT is reflection across the halfway point of the unsigned integers range
    val epsilon = scala.math.pow(2, s.length).toInt - 1 - gamma

    (gamma * epsilon).toString
  }

  sealed trait BinaryTree {
    def insert(key: String) = BinaryTree.insert(this, key, 0)
    def traverseByWeight(most: Boolean) = BinaryTree.traverseByWeight(this, most)
  }

  sealed trait WeightedNode {
    def n: Int
  }

  case class Node(l: BinaryTree, r: BinaryTree, n: Int) extends BinaryTree with WeightedNode
  case class Leaf(v: String) extends BinaryTree with WeightedNode {
    override def n = 1
  }
  case object Empty extends BinaryTree


  object BinaryTree {
    def apply(): BinaryTree = Empty

    private def insert(root: BinaryTree, v: String, s: Int) : BinaryTree = {
      root match {
        case Empty if v.length == s => Leaf(v)
        case Empty => insert(Node(Empty, Empty, 0), v, s)
        case Leaf(_) => root
        case Node(l, r, n) if v(s) == '0' => Node(insert(l, v, s + 1), r, n + 1)
        case Node(l, r, n) => Node(l, insert(r, v, s + 1), n +1)
      }
    }

    private def traverseByWeight(root: BinaryTree, most: Boolean) : Option[String] = {
      root match {
        case Empty => None // Shouldn't happen
        case Leaf(v) => Some(v)
        case Node(Empty, n, _) => traverseByWeight(n, most)
        case Node(n, Empty, _) => traverseByWeight(n, most)
        case Node(l: WeightedNode, r: WeightedNode, _) => {
          // I'm sure you coudl simplify this but I don't want to
          if (most) {
            if (r.n >= l.n) {
              traverseByWeight(r, most)
            } else {
              traverseByWeight(l, most)
            }
          } else {
            if (l.n <= r.n) {
              traverseByWeight(l, most)
            } else {
              traverseByWeight(r, most)
            }
          }
        }
      case _ => None
      }
    }
  }

  def parseBinaryString(s: String): Option[Int] = allCatch.opt(Integer.parseInt(s, 2))

  def problem2(data: String) : String = {
    val lines = data.split("\\n")

    val tree = lines.foldLeft(BinaryTree())(_.insert(_))

    val outcome = for {
      o2_string <- tree.traverseByWeight(true)
      co2_string <- tree.traverseByWeight(false)
      o2 <- parseBinaryString(o2_string)
      co2 <- parseBinaryString(co2_string)
      outcome = o2 * co2
    } yield outcome

    outcome match {
      case Some(v) => v.toString
      case None => "Some error occurred"
    }
  }
}
