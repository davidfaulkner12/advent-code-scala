package aoc2021.days

import java.math.BigInteger

object Day16 extends AocDay {

  val hexToBinary = Map(
    '0' -> "0000",
    '1' -> "0001",
    '2' -> "0010",
    '3' -> "0011",
    '4' -> "0100",
    '5' -> "0101",
    '6' -> "0110",
    '7' -> "0111",
    '8' -> "1000",
    '9' -> "1001",
    'A' -> "1010",
    'B' -> "1011",
    'C' -> "1100",
    'D' -> "1101",
    'E' -> "1110",
    'F' -> "1111"
  )

  sealed trait Packet {
    def version: String
    def versionSum: Int
  }

  case object EmptyPacket extends Packet {
    def version = "XXX"
    def versionSum = 0
  }

  case class LiteralPacket(version: String, values: Seq[String]) extends Packet {
    def versionSum = Integer.parseInt(version, 2)

    def value = BigInt(values.mkString, 2)
  }

  case class OperatorPacket(version: String, operator: String, subPackets: Seq[Packet]) extends Packet {
    def versionSum = Integer.parseInt(version, 2) + subPackets.map(_.versionSum).sum
  }

  case class ParseState(packet: Packet, rest: String)

  def nibbles(s: String): (Seq[String], String) = {
    def nibblesAcc(acc: Seq[String], s: String): (Seq[String], String) = {
      val(c, nibble, rest) = (s.substring(0, 1), s.substring(1, 5), s.substring(5))
      if (c == "0")
      (acc :+ nibble, rest)
      else nibblesAcc(acc :+ nibble, rest)
    }

    nibblesAcc(Seq(), s)
  }

  def literalPacket(s: String): ParseState  = {
    val (version, "100", rest) = (s.substring(0,3), s.substring(3,6), s.substring(6))

    val (nibble_seq, afterPacket) = nibbles(rest)

    ParseState(LiteralPacket(version, nibble_seq), afterPacket)
  }

  def numberOfPackets(acc: Seq[Packet], s: String, n: Int): (Seq[Packet], String) = {
    if (n == 0) (acc, s)
    else {
      val ParseState(next, rest) = packet(s)
      numberOfPackets(acc :+ next, rest, n - 1)
    }
  }

  def countingOpPacket(s: String): ParseState = {
    val (version, op, "1", rawCount, rest) = (s.substring(0, 3), s.substring(3, 6), s.substring(6,7), s.substring(7, 18), s.substring(18))

    val count = Integer.parseInt(rawCount, 2)

    val (containedPackets, rest2) = numberOfPackets(Seq(), rest, count)

    ParseState(OperatorPacket(version, op, containedPackets), rest2)

  }

  def sequencePackets(acc: Seq[Packet], s: String): Seq[Packet] = {
    if (s.isEmpty) acc
    else {
      val ParseState(next, rest) = packet(s)
      sequencePackets(acc :+ next, rest)
    }

  }

  def lengthOpPacket(s: String): ParseState = {
    val (version, op, "0", rawCount, rest) = (s.substring(0,3), s.substring(3, 6), s.substring(6, 7), s.substring(7, 22), s.substring(22))

    val count = Integer.parseInt(rawCount, 2)

    val (rawContainedPackets, rest2) = (rest.substring(0, count), rest.substring(count))

    val containedPackets = sequencePackets(Seq(), rawContainedPackets)

    ParseState(OperatorPacket(version, op, containedPackets), rest2)
  }

  def packet(s: String): ParseState = {
    val (op, opType) = (s.substring(3, 6), s(6))
    (op, opType) match {
      case ("100", _) => literalPacket(s)
      case (_, '0') => lengthOpPacket(s)
      case (_, '1') => countingOpPacket(s)
      case _ => ParseState(EmptyPacket, "")
    }
  }

  def evaluate(p: Packet): BigInt = p match {
    case l: LiteralPacket => l.value
    case OperatorPacket(_, "000", ps) => ps.map(evaluate(_)).sum
    case OperatorPacket(_, "001", ps) => ps.map(evaluate(_)).reduce(_ * _)
    case OperatorPacket(_, "010", ps) => ps.map(evaluate(_)).min
    case OperatorPacket(_, "011", ps) => ps.map(evaluate(_)).max
    case OperatorPacket(_, "101", Seq(p1, p2)) => if (evaluate(p1) > evaluate(p2)) 1 else 0
    case OperatorPacket(_, "110", Seq(p1, p2)) => if (evaluate(p1) < evaluate(p2)) 1 else 0
    case OperatorPacket(_, "111", Seq(p1, p2)) => if (evaluate(p1) == evaluate(p2)) 1 else 0
  }

  def problem1(rawData: String) : String = {
    val data = rawData.trim.map(hexToBinary(_)).mkString
    packet(data).packet.versionSum.toString
  }

  def problem2(rawData: String) : String = {
    val data = rawData.trim.map(hexToBinary(_)).mkString
    evaluate(packet(data).packet).toString
  }
}
