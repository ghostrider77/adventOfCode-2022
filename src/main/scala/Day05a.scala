object Day05a:
  import scala.annotation.tailrec

  final case class Move(idFrom: Int, idTo: Int, nrItems: Int)

  private def parseCrates(lines: List[String]): Vector[List[Char]] =
    val numberLine: String = lines.head
    val nrStacks: Int = numberLine.stripLeading().split("\\s+").length

    @tailrec
    def loop(crates: Vector[List[Char]], ls: List[String]): Vector[List[Char]] = ls match
      case Nil => crates
      case line :: lss =>
        val updatedCrates: Vector[List[Char]] = line.zip(numberLine).foldLeft(crates) {
          case (acc, (item, idChar)) =>
            if idChar.isDigit && item.isLetter then
              val id: Int = idChar.asDigit
              acc.updated(id - 1, item :: acc(id - 1))
            else acc
        }
        loop(updatedCrates, lss)

    loop(Vector.fill(nrStacks)(List.empty[Char]), lines.tail)

  private def parseMoves(lines: Iterator[String]): List[Move] =
    def parseLine(line: String): Move = line match
      case s"move $nrItems from $idFrom to $idTo" => Move(idFrom.toInt, idTo.toInt, nrItems.toInt)
      case _ => throw new Exception(s"Malformed input $line.")

    lines.map(parseLine).toList

  private def parseInputData(lines: Iterator[String]): (Vector[List[Char]], List[Move]) =
    val (crates, moves): (Iterator[String], Iterator[String]) = lines.span(_.nonEmpty)
    (parseCrates(crates.toList.reverse), parseMoves(moves.drop(1)))

  private def performRearrangement(crates: Vector[List[Char]], moves: List[Move]): Vector[List[Char]] =
    @tailrec
    def loop(acc: Vector[List[Char]], ms: List[Move]): Vector[List[Char]] = ms match
      case Nil => acc
      case (move @ Move(from, to, nrItems)) :: mss =>
        if nrItems == 0 then loop(acc, mss)
        else
          val stack: List[Char] = acc(from - 1)
          val updated: Vector[List[Char]] = acc.updated(from - 1, stack.tail).updated(to - 1, stack.head :: acc(to - 1))
          loop(updated, move.copy(nrItems = nrItems - 1) :: mss)

    loop(crates, moves)

  def getTopItems(crates: Vector[List[Char]], moves: List[Move]): String =
    def getTopItem(crate: List[Char]): String = crate match
      case Nil => ""
      case x :: _ => x.toString

    val rearrangedCrates: Vector[List[Char]] = performRearrangement(crates, moves)
    rearrangedCrates.map(getTopItem).mkString

  def main(args: Array[String]): Unit =
    val lines: Iterator[String] = scala.io.Source.fromResource("input05.txt").getLines()
    val (crates, moves): (Vector[List[Char]], List[Move]) = parseInputData(lines)
    val result: String = getTopItems(crates, moves)
    println(result)

end Day05a
