package aoc2021.days

import utest._
import aoc2021.days.Day3._

object Day3Tests extends TestSuite {
  val tests = Tests{

    val example = """00100
                    |11110
                    |10110
                    |10111
                    |10101
                    |01111
                    |00111
                    |11100
                    |10000
                    |11001
                    |00010
                    |01010""".stripMargin

    test("testExampleProb1") {
      assert("198" == problem1(example))
    }

    test("binaryTree constructs") {
      val b = BinaryTree()
      assert(Empty == b)
    }

    test("binaryTree inserts") {
      val b0 = BinaryTree()
      val b1 = b0.insert("1")

      val Node(l1, r1, n1) = b1
      assert(1 == n1)
      assert(Empty == l1)
      assert(Leaf("1") == r1)
    }

    test("binaryTree counts") {
      val b = BinaryTree().insert("1").insert("0")

      val Node(l1, r1, n1) = b
      assert(2 == n1)
      assert(Leaf("0") == l1)
      assert(Leaf("1") == r1)
    }


    test("binaryTree recurses") {
      val b = BinaryTree().insert("111").insert("101").insert("011")

      val Node(l1, r1, n1) = b
      assert(3 == n1)

      assert(2 == r1.asInstanceOf[Node].n)

      assert(1 == l1.asInstanceOf[Node].n)

    }

    test("binaryTree holds example") {
      val b = example.split("\\n").foldLeft(BinaryTree())(_.insert(_))

      // Just testing no exception
    }

    val exampleTree = example.split("\\n").foldLeft(BinaryTree())(_.insert(_))

    test("binary tree traverse example most") {
      val Some(x) = exampleTree .traverseByWeight(true)

      assert("10111" == x)
    }

    test("binary tree traverse example least") {
      val Some(x) = exampleTree.traverseByWeight(false)

      assert("01010" == x)
    }

    test("problem2") {
      assert("230" == problem2(example))
    }

  }
}
