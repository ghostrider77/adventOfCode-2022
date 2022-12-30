object Day22a:
  import scala.annotation.tailrec
  type Move = Either[Int, Turn]

  enum Turn:
    case LeftTurn, RightTurn

  object Turn:
    def apply(c: Char): Turn = c match
      case 'L' => LeftTurn
      case 'R' => RightTurn
      case _ => throw Exception(s"Unknown turn $c.")

  enum Direction:
    case North, East, South, West

    def score: Int = this match
      case East => 0
      case South => 1
      case West => 2
      case North => 3

    def directionAfterTurn(turn: Turn): Direction = (turn, this) match
      case (Turn.LeftTurn, North) | (Turn.RightTurn, South) => West
      case (Turn.LeftTurn, East) | (Turn.RightTurn, West) => North
      case (Turn.LeftTurn, South) | (Turn.RightTurn, North) => East
      case (Turn.LeftTurn, West) | (Turn.RightTurn, East) => South

  enum BoardCell:
    case OpenTile, Wall, Empty

  object BoardCell:
    def apply(c: Char): BoardCell = c match
      case '.' => OpenTile
      case '#' => Wall
      case ' ' => Empty
      case _ => throw Exception(s"Unknown board cell ${c.toString}.")

  case class Cell(x: Int, y: Int)

  case class Position(position: Cell, direction: Direction)

  case class Board(board: Map[Cell, BoardCell]):
    val startCell: Cell = board.keysIterator.filter{ case Cell(_, y) => y == 1 }.minBy(_.x)

    private def nextCellOnMap(cell: Cell, direction: Direction): Cell =
      val Cell(x, y) = cell
      direction match
        case Direction.North =>
          val next: Cell = Cell(x, y - 1)
          if board.contains(next) then next else board.keysIterator.filter{ case Cell(x0, _) => x0 == x }.maxBy(_.y)
        case Direction.East =>
          val next: Cell = Cell(x + 1, y)
          if board.contains(next) then next else board.keysIterator.filter{ case Cell(_, y0) => y0 == y }.minBy(_.x)
        case Direction.South =>
          val next: Cell = Cell(x, y + 1)
          if board.contains(next) then next else board.keysIterator.filter{ case Cell(x0, _) => x0 == x }.minBy(_.y)
        case Direction.West =>
          val next: Cell = Cell(x - 1, y)
          if board.contains(next) then next else board.keysIterator.filter{ case Cell(_, y0) => y0 == y }.maxBy(_.x)

    def nextTileAhead(cell: Cell, direction: Direction): (Cell, BoardCell) =
      val nextCell: Cell = nextCellOnMap(cell, direction)
      (nextCell, board(nextCell))

  private def parseMoves(line: String): List[Move] =
    @tailrec
    def loop(acc: List[Move], ls: List[Char]): List[Move] = ls.headOption match
      case None => acc.reverse
      case Some(char) if char.isLetter => loop(Right(Turn(char)) :: acc, ls.tail)
      case _ =>
        val (digits, rest): (List[Char], List[Char]) = ls.span(_.isDigit)
        loop(Left(digits.mkString.toInt) :: acc, rest)

    loop(Nil, line.toList)

  def readInput(lines: Iterator[String]): (Board, List[Move]) =
    val (boardLines, rest): (Iterator[String], Iterator[String]) = lines.span(_.nonEmpty)
    val board: Map[Cell, BoardCell] =
      (for
        (line, jy) <- boardLines.zipWithIndex
        (char, ix) <- line.zipWithIndex
        cell: BoardCell = BoardCell(char)
        if cell != BoardCell.Empty
      yield Cell(ix + 1, jy + 1) -> cell).toMap

    (Board(board), parseMoves(rest.drop(1).next()))

  private def goStraight(board: Board, position: Position, nrSteps: Int): Cell =
    val Position(cell, direction) = position
    @tailrec
    def loop(currentCell: Cell, k: Int): Cell =
      if k == nrSteps then currentCell
      else
        val (nextCell, cellType): (Cell, BoardCell) = board.nextTileAhead(currentCell, direction)
        if cellType == BoardCell.Wall then currentCell
        else loop(nextCell, k + 1)

    loop(cell, 0)

  def followThePath(board: Board, moves: List[Move]): Int =
    @tailrec
    def loop(position: Position, ms: List[Move]): Position =
      val Position(_, direction) = position
      ms match
        case Nil => position
        case move :: mss => move match
          case Left(steps) =>
            val nextCell: Cell = goStraight(board, position, steps)
            loop(position.copy(position = nextCell), mss)
          case Right(turn) => loop(position.copy(direction = direction.directionAfterTurn(turn)), mss)

    val Position(Cell(x, y), direction) = loop(Position(board.startCell, direction = Direction.East), moves)
    1000 * y + 4 * x + direction.score

  def main(args: Array[String]): Unit =
    val lines: Iterator[String] = scala.io.Source.fromResource("input22.txt").getLines()
    val (board, moves): (Board, List[Move]) = readInput(lines)
    val result: Int = followThePath(board, moves)
    println(result)

end Day22a
