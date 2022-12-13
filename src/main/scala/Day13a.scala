object Day13a:
  import scala.annotation.tailrec

  sealed trait Packet
  case class Value(n: Int) extends Packet
  case class PacketList(packets: List[Packet]) extends Packet

  sealed trait ParsingResult
  case object Bracket extends ParsingResult
  case class ParsedPacket(packet: Packet) extends ParsingResult

  private def parsePacket(line: String): Packet =
    @tailrec
    def loop(xs: List[String], stack: List[ParsingResult]): Packet = xs match
      case Nil => stack.collect{ case p: ParsedPacket => p.packet }.head
      case "," :: xss => loop(xss, stack)
      case "[" :: xss => loop(xss, Bracket :: stack)
      case "]" :: xss =>
        val (group, rest): (List[ParsingResult], List[ParsingResult]) = stack.span(_ != Bracket)
        loop(xss, ParsedPacket(PacketList(group.collect{ case p: ParsedPacket => p.packet }.reverse)) :: rest.drop(1))
      case _ =>
        val (item, rest): (List[String], List[String]) = xs.span(s => s != "," && s != "]")
        loop(rest.dropWhile(_ == ","), ParsedPacket(Value(item.mkString.toInt)) :: stack)

    loop(line.toList.map(_.toString), Nil)

  def parseInput(lines: Iterator[String]): List[(Packet, Packet)] =
    @tailrec
    def loop(acc: List[(Packet, Packet)], ls: Iterator[String]): List[(Packet, Packet)] =
      val (pair, rest): (Iterator[String], Iterator[String]) = ls.span(_.nonEmpty)
      pair.toList match
        case List(p1, p2) =>
          loop((parsePacket(p1), parsePacket(p2)) :: acc, rest.drop(1))
        case _ => acc.reverse

    loop(Nil, lines)

  private def isCorrectOrder(left: Packet, right: Packet): Boolean = (left, right) match
    case (Value(l), Value(r)) => l < r
    case (PacketList(l), PacketList(r)) =>
      @tailrec
      def loop(ls: List[Packet], rs: List[Packet]): Boolean = (ls, rs) match
        case (x :: lss, y :: rss) => if x == y then loop(lss, rss) else isCorrectOrder(x, y)
        case _ => rs.nonEmpty
      loop(l, r)
    case (PacketList(_), _) => isCorrectOrder(left, PacketList(List(right)))
    case (_, PacketList(_)) => isCorrectOrder(PacketList(List(left)), right)

  def findIndicesOfCorrectlyOrderedPairs(packetPairs: List[(Packet, Packet)]): Int =
    packetPairs
      .zipWithIndex
      .filter{ case ((p1, p2), _) => isCorrectOrder(p1, p2) }
      .map((_, ix) => ix + 1)
      .sum

  def main(args: Array[String]): Unit =
    val lines: Iterator[String] = scala.io.Source.fromResource("input13.txt").getLines()
    val packetPairs: List[(Packet, Packet)] = parseInput(lines)
    val result: Int = findIndicesOfCorrectlyOrderedPairs(packetPairs)
    println(result)

end Day13a
