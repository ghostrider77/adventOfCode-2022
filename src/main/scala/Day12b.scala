object Day12b:
  import scala.annotation.tailrec
  import scala.collection.mutable.{Queue as MutableQueue, Map as MutableMap}

  case class Cell(x: Int, y: Int)

  case class HeightMap(heightMap: Map[Cell, Char]):
    def neighbours(cell: Cell): List[Cell] =
      val Cell(i, j) = cell
      val height: Char = heightMap(cell)
      val cellsAround: List[Cell] = List(Cell(i + 1, j), Cell(i - 1, j), Cell(i, j + 1), Cell(i, j - 1))

      def isNeighbour(c: Cell): Boolean =
        heightMap.get(c) match
          case None => false
          case Some('Z') => true
          case Some(n) => n <= height + 1

      cellsAround.filter(isNeighbour)

  private def breadthFirstSearch(heightMap: HeightMap, start: Cell, end: Cell): Int =
    val distances: MutableMap[Cell, Int] = MutableMap(start -> 0)
    val queue: MutableQueue[Cell] = MutableQueue(start)
    while queue.nonEmpty do
      val cell: Cell = queue.dequeue()
      val neighbours: List[Cell] = heightMap.neighbours(cell)
      neighbours.foreach { neighbour =>
        if !distances.contains(neighbour) then
          queue.enqueue(neighbour)
          distances(neighbour) = distances(cell) + 1
      }
    end while
    distances.getOrElse(end, Int.MaxValue)

  def parseHeightMap(lines: Iterator[String]): (HeightMap, List[Cell], Cell) =
    val chars: List[(Cell, Char)] =
      lines
        .zipWithIndex
        .flatMap((line, y) => line.zipWithIndex.map((char, x) => (Cell(x, y), char)))
        .toList

    val start: Cell = chars.find((_, char) => char == 'S').get._1
    val end: Cell = chars.find((_, char) => char == 'E').get._1
    val heights: Map[Cell, Char] = chars.toMap ++ Map(start -> 'a', end -> 'z')
    val starts: List[Cell] = heights.filter((_, char) => char == 'a').keysIterator.toList
    (HeightMap(heights), starts, end)

  def findShortestHikingPath(heightMap: HeightMap, startCells: List[Cell], end: Cell): Int =
    @tailrec
    def loop(acc: Int, xs: List[Cell]): Int = xs match
      case Nil => acc
      case start :: rest =>
        val distance: Int = breadthFirstSearch(heightMap, start, end)
        loop(math.min(distance, acc), rest)

    loop(Int.MaxValue, startCells)

  def main(args: Array[String]): Unit =
    val lines: Iterator[String] = scala.io.Source.fromResource("input12.txt").getLines()
    val (heightMap, starts, end): (HeightMap, List[Cell], Cell) = parseHeightMap(lines)
    val result: Int = findShortestHikingPath(heightMap, starts, end)
    println(result)

end Day12b

