object Day14b:
  import scala.annotation.tailrec

  case class Cell(x: Int, y: Int)
  case class Cave(units: Set[Cell], yMax: Int)

  private def parseLine(line: String): Set[Cell] =
    def pointsOfSegment(c1: Cell, c2: Cell): Set[Cell] =
      if c1.x == c2.x then
        if c1.y <= c2.y then (c1.y to c2.y).map(y => Cell(c1.x, y)).toSet
        else (c1.y to c2.y by -1).map(y => Cell(c1.x, y)).toSet
      else if c1.x < c2.x then (c1.x to c2.x).map(x => Cell(x, c1.y)).toSet
      else (c1.x to c2.x by -1).map(x => Cell(x, c1.y)).toSet

    val intervals: Iterator[List[(Int, Int)]] =
      line
        .split(" -> ")
        .toList
        .map(_.split(",").map(_.toInt).toList)
        .collect { case List(a, b) => (a, b) }
        .sliding(2)

    (for List((a, b), (c, d)) <- intervals yield pointsOfSegment(Cell(a, b), Cell(c, d))).flatten.toSet

  def parseInputData(lines: Iterator[String]): Cave =
    val cells: Set[Cell] = lines.flatMap(parseLine).toSet
    val yMax: Int = cells.map(_.y).max + 2
    Cave(cells, yMax)

  private def pourSandUntilItRests(cave: Cave, source: Cell): Option[Cave] =
    val Cave(caveUnits, yMax) = cave

    @tailrec
    def loop(sandPosition: Cell): Option[Cell] =
        val Cell(x, y) = sandPosition
        val possibleOptions: List[Cell] = List(Cell(x, y + 1), Cell(x - 1, y + 1), Cell(x + 1, y + 1))
        possibleOptions.filter(_.y < yMax).find(!caveUnits.contains(_)) match
          case None => Some(sandPosition)
          case Some(nextPosition) => loop(nextPosition)

    loop(source) match
      case None => None
      case Some(restPosition) =>
        if restPosition == source then None
        else Some(cave.copy(units = caveUnits + restPosition))

  def pourSandIntoCave(cave: Cave, source: Cell): Int =
    @tailrec
    def loop(acc: Cave, nrSandUnits: Int): Int =
      pourSandUntilItRests(acc, source) match
        case None => nrSandUnits
        case Some(updatedCave) => loop(updatedCave, nrSandUnits + 1)

    loop(cave, 0) + 1

  def main(args: Array[String]): Unit =
    val lines: Iterator[String] = scala.io.Source.fromResource("input14.txt").getLines()
    val cave: Cave = parseInputData(lines)
    val source = Cell(500, 0)
    val result: Int = pourSandIntoCave(cave, source)
    println(result)

end Day14b
