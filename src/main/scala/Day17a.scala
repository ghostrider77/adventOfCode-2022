object Day17a:
  import scala.annotation.tailrec

  case class Cell(x: Int, y: Int)
  case class Chamber(rocks: Set[Cell], xMin: Int, xMax: Int, yMin: Int)

  enum Direction:
    case Left, Right, Down

  sealed trait Shape:
    val topLeft: Cell
    private val Cell(x, y) = topLeft
    val coordinates: Set[Cell] = this match
      case Minus(_) => Set(Cell(x, y), Cell(x + 1, y), Cell(x + 2, y), Cell(x + 3, y))
      case Plus(_) => Set(Cell(x + 1, y), Cell(x, y - 1), Cell(x + 1, y - 1), Cell(x + 2, y - 1), Cell(x + 1, y - 2))
      case Corner(_) => Set(Cell(x + 2, y), Cell(x + 2, y - 1), Cell(x, y - 2), Cell(x + 1, y - 2), Cell(x + 2, y - 2))
      case Vertical(_) => Set(Cell(x, y), Cell(x, y - 1), Cell(x, y - 2), Cell(x, y - 3))
      case Square(_) => Set(Cell(x, y), Cell(x + 1, y), Cell(x, y - 1), Cell(x + 1, y - 1))

    def next(xMin: Int, yBottom: Int): Shape = this match
      case Minus(_) => Plus(Cell(xMin, yBottom + 2))
      case Plus(_) => Corner(Cell(xMin, yBottom + 2))
      case Corner(_) => Vertical(Cell(xMin, yBottom + 3))
      case Vertical(_) => Square(Cell(xMin, yBottom + 1))
      case Square(_) => Minus(Cell(xMin, yBottom))
  end Shape

  case class Minus(topLeft: Cell) extends Shape
  case class Plus(topLeft: Cell) extends Shape
  case class Corner(topLeft: Cell) extends Shape
  case class Vertical(topLeft: Cell) extends Shape
  case class Square(topLeft: Cell) extends Shape

  enum JetPattern:
    case Left, Right

  object JetPattern:
    def apply(s: Char): JetPattern = s match
      case '>' => Right
      case '<' => Left
      case _ => throw Exception(s"Unknown jet pattern $s.")

  def readInput(line: String): Iterator[JetPattern] =
    Iterator.continually(line.toVector.map(JetPattern(_))).flatten

  private def moveShape(shape: Shape, direction: Direction): Shape =
    val Cell(x, y) = shape.topLeft
    val topLeft: Cell = direction match
      case Direction.Left => Cell(x - 1, y)
      case Direction.Right => Cell(x + 1, y)
      case Direction.Down => Cell(x, y - 1)
    shape match
      case Minus(_) => Minus(topLeft)
      case Plus(_) => Plus(topLeft)
      case Corner(_) => Corner(topLeft)
      case Vertical(_) => Vertical(topLeft)
      case Square(_) => Square(topLeft)

  private def isValidPosition(shape: Shape, chamber: Chamber): Boolean =
    val Chamber(rocks, xMin, xMax, yMin) = chamber
    shape.coordinates.map(_.x).forall(x => xMin <= x && x <= xMax) &&
      shape.coordinates.map(_.y).forall(y => y >= yMin) &&
      shape.coordinates.forall(!rocks.contains(_))

  private def rockFall(shape: Shape, chamber: Chamber, jets: Iterator[JetPattern]): Chamber =
    @tailrec
    def loop(current: Shape): Chamber =
      val jet: JetPattern = jets.next()
      val direction: Direction = if jet == JetPattern.Left then Direction.Left else Direction.Right
      val pushed: Shape = moveShape(current, direction)
      val pushedShape: Shape = if isValidPosition(pushed, chamber) then pushed else current
      val downwardShape: Shape = moveShape(pushedShape, Direction.Down)
      if isValidPosition(downwardShape, chamber) then loop(downwardShape)
      else chamber.copy(rocks = chamber.rocks ++ pushedShape.coordinates)

    loop(shape)

  private def getChamberHeight(chamber: Chamber): Int =
    chamber.rocks.maxByOption(_.y) match
      case None => 0
      case Some(Cell(_, y)) => y

  def calcRockTowerHeight(nrRocks: Int, jets: Iterator[JetPattern]): Int =
    @tailrec
    def loop(currentShape: Shape, chamber: Chamber, k: Int): Chamber =
      if k > nrRocks then chamber
      else
        val updatedChamber: Chamber = rockFall(currentShape, chamber, jets)
        val yMax: Int = getChamberHeight(updatedChamber)
        val nextShape: Shape = currentShape.next(xMin = 2, yBottom = yMax + 4)
        loop(nextShape, updatedChamber, k + 1)

    val initialChamber = Chamber(Set.empty[Cell], xMin = 0, xMax = 6, yMin = 0)
    val firstShape: Shape = Minus(topLeft = Cell(x = 2, y = 3))
    val finalChamber: Chamber = loop(firstShape, initialChamber, k = 1)
    getChamberHeight(finalChamber) + 1

  def main(args: Array[String]): Unit =
    val lines: Iterator[String] = scala.io.Source.fromResource("input17.txt").getLines()
    val jetPatterns: Iterator[JetPattern] = readInput(lines.next())
    val nrRocks: Int = 2022
    val result: Int = calcRockTowerHeight(nrRocks, jetPatterns)
    println(result)

end Day17a
