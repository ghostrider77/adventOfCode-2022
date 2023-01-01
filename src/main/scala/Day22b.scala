object Day22b:
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

  case class Cell(x: Int, y: Int):
    def neighbour(direction: Direction): Cell = direction match
      case Direction.East => Cell(x + 1, y)
      case Direction.South => Cell(x, y + 1)
      case Direction.West => Cell(x - 1, y)
      case Direction.North => Cell(x, y - 1)

  case class Position(position: Cell, direction: Direction)

  case class Board(board: Map[Cell, BoardCell], faceSize: Int):
    val startCell: Cell = board.keysIterator.filter{ case Cell(_, y) => y == 1 }.minBy(_.x)

    private def nextCellOnMap(cell: Cell, direction: Direction): (Cell, Direction) =
      val Cell(x, y) = cell
      val neighbour: Cell = cell.neighbour(direction)
      if board.contains(neighbour) then (neighbour, direction)
      else direction match
        case Direction.North =>
          if x <= faceSize then (Cell(faceSize + 1, faceSize + x), Direction.East)
          else if x <= 2*faceSize then (Cell(1, 2*faceSize + x), Direction.East)
          else (Cell(x - 2*faceSize, 4*faceSize), Direction.North)
        case Direction.East =>
          if y <= faceSize then (Cell(2*faceSize, 3*faceSize + 1 - y), Direction.West)
          else if y <= 2*faceSize then (Cell(y + faceSize, faceSize), Direction.North)
          else if y <= 3*faceSize then (Cell(3*faceSize, 3*faceSize + 1 - y), Direction.West)
          else (Cell(y - 2*faceSize, 3*faceSize), Direction.North)
        case Direction.South =>
          if x <= faceSize then (Cell(x + 2*faceSize, 1), Direction.South)
          else if x <= 2*faceSize then (Cell(faceSize, x + 2*faceSize), Direction.West)
          else (Cell(2*faceSize, x - faceSize), Direction.West)
        case Direction.West =>
          if y <= faceSize then (Cell(1, 3*faceSize + 1 - y), Direction.East)
          else if y <= 2*faceSize then (Cell(y - faceSize, 2*faceSize + 1), Direction.South)
          else if y <= 3*faceSize then (Cell(faceSize + 1, 3*faceSize + 1 - y), Direction.East)
          else (Cell(y - 2*faceSize, 1), Direction.South)

    def nextTileAhead(cell: Cell, direction: Direction): (Cell, BoardCell, Direction) =
      val (nextCell, nextDirection): (Cell, Direction) = nextCellOnMap(cell, direction)
      (nextCell, board(nextCell), nextDirection)

  private def parseMoves(line: String): List[Move] =
    @tailrec
    def loop(acc: List[Move], ls: List[Char]): List[Move] = ls.headOption match
      case None => acc.reverse
      case Some(char) if char.isLetter => loop(Right(Turn(char)) :: acc, ls.tail)
      case _ =>
        val (digits, rest): (List[Char], List[Char]) = ls.span(_.isDigit)
        loop(Left(digits.mkString.toInt) :: acc, rest)

    loop(Nil, line.toList)

  def readInput(lines: List[String], faceSize: Int): (Board, List[Move]) =
    val (boardLines, rest): (List[String], List[String]) = lines.span(_.nonEmpty)
    val board: Map[Cell, BoardCell] =
      (for
        (line, jy) <- boardLines.zipWithIndex
        (char, ix) <- line.zipWithIndex
        cell: BoardCell = BoardCell(char)
        if cell != BoardCell.Empty
      yield Cell(ix + 1, jy + 1) -> cell).toMap

    (Board(board, faceSize), parseMoves(rest.last))

  def followThePath(board: Board, moves: List[Move]): Int =
    @tailrec
    def loop(position: Position, ms: List[Move]): Position =
      val Position(currentCell, direction) = position
      ms match
        case Nil => position
        case move :: mss => move match
          case Left(0) => loop(position, mss)
          case Left(steps) =>
            val (nextCell, cellType, nextDirection): (Cell, BoardCell, Direction) =
              board.nextTileAhead(currentCell, direction)
            if cellType == BoardCell.Wall then loop(position, mss)
            else loop(Position(nextCell, nextDirection), Left(steps - 1) :: mss)
          case Right(turn) => loop(position.copy(direction = direction.directionAfterTurn(turn)), mss)

    val Position(Cell(x, y), direction) = loop(Position(board.startCell, direction = Direction.East), moves)
    1000 * y + 4 * x + direction.score

  def main(args: Array[String]): Unit =
    val lines: Iterator[String] = scala.io.Source.fromResource("input22.txt").getLines()
    val faceSize: Int = 50
    val (board, moves): (Board, List[Move]) = readInput(lines.toList, faceSize)
    val result: Int = followThePath(board, moves)
    println(result)

end Day22b
