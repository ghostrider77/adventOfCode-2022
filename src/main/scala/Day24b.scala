object Day24b:
  import scala.annotation.tailrec
  import scala.collection.mutable.Queue as MutableQueue

  extension (n: Int)
    private def mod(m: Int): Int =
      val r: Int = n % m
      if r < 0 then r + m else r

  case class Cell(x: Int, y: Int)
  case class Position(cell: Cell, t: Int)

  enum Blizzard:
    case LeftMoving, RightMoving, UpwardMoving, DownwardMoving

  object Blizzard:
    def apply(c: Char): Blizzard = c match
      case '>' => RightMoving
      case '<' => LeftMoving
      case '^' => UpwardMoving
      case 'v' => DownwardMoving
      case _ => throw Exception(s"Unknown wind direction $c.")

  case class ValleyMap(initialState: Map[Cell, Blizzard], nrRows: Int, nrCols: Int):
    val entrance: Cell = Cell(x = 0, y = -1)
    val exit: Cell = Cell(x = nrCols - 1, y = nrRows)

    private def caughtByWind(cell: Cell, blizzard: Blizzard): Boolean =
      initialState.get(cell).contains(blizzard)

    def isBlizzardFree(position: Position): Boolean =
      val Position(Cell(x, y), t) = position
      val left: Cell = Cell((x - t).mod(nrCols), y)
      val right: Cell = Cell((x + t).mod(nrCols), y)
      val up: Cell = Cell(x, (y - t).mod(nrRows))
      val down: Cell = Cell(x, (y + t).mod(nrRows))
      !caughtByWind(left, Blizzard.RightMoving) && !caughtByWind(right, Blizzard.LeftMoving) &&
        !caughtByWind(up, Blizzard.DownwardMoving) && !caughtByWind(down, Blizzard.UpwardMoving)

    private def isInValley(position: Position): Boolean =
      val Position(cell @ Cell(x, y), _) = position
      (0 <= x && x < nrCols && 0 <= y && y < nrRows) || cell == entrance || cell == exit

    def neighbours(position: Position): List[Position] =
      val Position(Cell(x, y), t) = position
      val candidates: Iterator[Position] =
        Iterator(
          Position(Cell(x + 1, y), t + 1),
          Position(Cell(x - 1, y), t + 1),
          Position(Cell(x, y + 1), t + 1),
          Position(Cell(x, y - 1), t + 1),
          Position(Cell(x, y), t + 1)
        )
      candidates.filter(isInValley).toList

  private def findShortestPath(valleyMap: ValleyMap, startCell: Cell, endCell: Cell, startTime: Int): Int =
    val start: Position = Position(startCell, startTime)
    val queue: MutableQueue[Position] = MutableQueue(start)

    @tailrec
    def loop(visited: Set[Position]): Int =
      if queue.isEmpty then throw Exception("Valley exit is unreachable.")
      else
        val position @ Position(cell, t) = queue.dequeue()
        if cell == endCell then t
        else
          val neighbours: List[Position] =
            valleyMap.neighbours(position).filter(n => valleyMap.isBlizzardFree(n) && !visited.contains(n))
          queue.enqueueAll(neighbours)
          loop(visited ++ neighbours)

    loop(Set(start))

  def readInput(lines: List[String]): ValleyMap =
    val rows: List[String] = lines.tail.dropRight(1).map(_.drop(1).dropRight(1))
    val nrRows: Int = rows.length
    val nrCols: Int = rows.head.length
    val blizzards: Map[Cell, Blizzard] =
      (for
        (row, jy) <- rows.zipWithIndex
        (p, ix) <- row.zipWithIndex
        if p != '.'
      yield Cell(ix, jy) -> Blizzard(p)).toMap

    ValleyMap(blizzards, nrRows, nrCols)

  def roundTrip(valleyMap: ValleyMap): Int =
    val t0: Int = findShortestPath(valleyMap, valleyMap.entrance, valleyMap.exit, startTime = 0)
    val t1: Int = findShortestPath(valleyMap, valleyMap.exit, valleyMap.entrance, startTime = t0)
    findShortestPath(valleyMap, valleyMap.entrance, valleyMap.exit, startTime = t1)

  def main(args: Array[String]): Unit =
    val lines: Iterator[String] = scala.io.Source.fromResource("input24.txt").getLines()
    val valley: ValleyMap = readInput(lines.toList)
    val result: Int = roundTrip(valley)
    println(result)

end Day24b
