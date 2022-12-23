object Day23a:
  import scala.annotation.tailrec
  import scala.collection.mutable.Queue as MutableQueue

  case class Cell(x: Int, y: Int)

  enum Direction:
    case North, South, West, East

  def readInput(lines: Iterator[String]): Set[Cell] =
    lines
      .zipWithIndex
      .flatMap{ case (line, y) => line.zipWithIndex.map{ case (char, x) => (Cell(x, y), char) } }
      .collect{ case (cell, char) if char == '#' => cell }
      .toSet

  private def getNrEmptyCellsInEnclosingRectangle(elves: Set[Cell]): Int =
    val xMin: Int = elves.map(_.x).min
    val xMax: Int = elves.map(_.x).max
    val yMin: Int = elves.map(_.y).min
    val yMax: Int = elves.map(_.y).max
    (xMax - xMin + 1) * (yMax - yMin + 1) - elves.size

  private def allNeighboursFree(elves: Set[Cell], elf: Cell): Boolean =
    val Cell(x, y) = elf
    val neighbours: List[Cell] =
      List(
        Cell(x, y - 1),
        Cell(x + 1, y - 1),
        Cell(x + 1, y),
        Cell(x + 1, y + 1),
        Cell(x, y + 1),
        Cell(x - 1, y + 1),
        Cell(x - 1, y),
        Cell(x - 1, y - 1)
      )
    neighbours.forall(!elves.contains(_))

  private def proposeNextStep(elves: Set[Cell], elf: Cell, direction: Direction): Option[Cell] =
    val Cell(x, y) = elf
    direction match
      case Direction.North =>
        val northCells: List[Cell] = List(Cell(x, y - 1), Cell(x + 1, y - 1), Cell(x - 1, y - 1))
        if northCells.exists(elves.contains) then None else Some(Cell(x, y - 1))
      case Direction.South =>
        val southCells: List[Cell] = List(Cell(x, y + 1), Cell(x + 1, y + 1), Cell(x - 1, y + 1))
        if southCells.exists(elves.contains) then None else Some(Cell(x, y + 1))
      case Direction.West =>
        val westCells: List[Cell] = List(Cell(x - 1, y), Cell(x - 1, y - 1), Cell(x - 1, y + 1))
        if westCells.exists(elves.contains) then None else Some(Cell(x - 1, y))
      case Direction.East =>
        val eastCells: List[Cell] = List(Cell(x + 1, y), Cell(x + 1, y - 1), Cell(x + 1, y + 1))
        if eastCells.exists(elves.contains) then None else Some(Cell(x + 1, y))

  private def getProposedCells(elves: Set[Cell], directions: List[Direction]): Map[Cell, Cell] =
    def getNextCell(elf: Cell): Option[Cell] =
      if allNeighboursFree(elves, elf) then None
      else directions.iterator.flatMap(proposeNextStep(elves, elf, _)).nextOption()

    (for
      elf <- elves
      nextCell <- getNextCell(elf)
    yield elf -> nextCell).toMap

  private def getNextPositions(elfToNextCell: Map[Cell, Cell]): Map[Cell, Cell] =
    val uniqueNextPositions: Set[Cell] =
      elfToNextCell
        .groupMapReduce{ case (_, proposed) => proposed }(_ => 1)(_ + _)
        .filter{ case (_, occurrence) => occurrence == 1 }
        .keySet
    elfToNextCell.filter{ case (_, p) => uniqueNextPositions.contains(p) }

  def calcNrEmptyGrounds(elves: Set[Cell], nrRounds: Int): Int =
    val queue: MutableQueue[Direction] = MutableQueue(Direction.North, Direction.South, Direction.West, Direction.East)

    @tailrec
    def loop(currentElfPositions: Set[Cell], k: Int): Int =
      if k == nrRounds then getNrEmptyCellsInEnclosingRectangle(currentElfPositions)
      else
        val directions: List[Direction] = queue.take(4).toList
        val nextCells: Map[Cell, Cell] = getProposedCells(currentElfPositions, directions)
        val actualNextCells: Map[Cell, Cell] = getNextPositions(nextCells)
        val firstDirection: Direction = queue.dequeue()
        queue.enqueue(firstDirection)
        loop(currentElfPositions.diff(actualNextCells.keySet) ++ actualNextCells.values, k + 1)

    loop(elves, 0)

  def main(args: Array[String]): Unit =
    val lines: Iterator[String] = scala.io.Source.fromResource("input23.txt").getLines()
    val elves: Set[Cell] = readInput(lines)
    val result: Int = calcNrEmptyGrounds(elves, nrRounds = 10)
    println(result)

end Day23a
