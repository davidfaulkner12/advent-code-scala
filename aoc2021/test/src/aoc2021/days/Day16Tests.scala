package aoc2021.days

import utest._
import aoc2021.days.Day16._
 import java.math.BigInteger

object Day16Tests extends TestSuite {
  val tests = Tests{

    test("testParser") {
      val (Seq("0000"), "1111") = nibbles("000001111")
      val ParseState(LiteralPacket("110", Seq("0111", "1110", "0101")), _) = packet("110100101111111000101000")
      val ParseState(OperatorPacket("111", "011", Seq(LiteralPacket("010", Seq("0001")), LiteralPacket("100", Seq("0010")), LiteralPacket("001", Seq("0011")))), _) = packet("11101110000000001101010000001100100000100011000001100000")
      val ParseState(OperatorPacket("001", "110", Seq(LiteralPacket("110", Seq("1010")), LiteralPacket("010", Seq("0001", "0100")))), _) = packet("00111000000000000110111101000101001010010001001000000000")
    }

    test("testFirstExamples") {
      val rawFirstExample = "8A004A801A8002F478"
      val firstExample = new BigInteger(rawFirstExample, 16).toString(2)
      val ParseState(p, _) = packet(firstExample)
      assert(16 == p.versionSum)
    }

    test("problem1") {
      assert("16" == problem1("8A004A801A8002F478"))
      println("here")
      assert("12" == problem1("620080001611562C8802118E34"))
      assert("23" == problem1("C0015000016115A2E0802F182340"))
      assert("31" == problem1("A0016C880162017C3686B18A3D4780"))
    }

    test("literalValue") {
      val ParseState(l, _) = packet("110100101111111000101000")
      assert(2021 == l.asInstanceOf[LiteralPacket].value)
    }

    test("evaluate") {
      assert("3" == problem2("C200B40A82"))
      assert("54" == problem2("04005AC33890"))
      assert("7" == problem2("880086C3E88112"))
      assert("9" == problem2("CE00C43D881120"))
      assert("1" == problem2("D8005AC2A8F0"))
      assert("0" == problem2("F600BC2D8F"))
      assert("0" == problem2("9C005AC2F8F0"))
      assert("1" == problem2("9C0141080250320F1802104A08"))
    }
  }
}


