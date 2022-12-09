object Day09a:
  import scala.annotation.tailrec

  enum Direction:
    case Left, Right, Up, Down

  object Direction:
    def apply(s: String): Direction = s match
      case "L" => Left
      case "R" => Right
      case "U" => Up
      case "D" => Down
      case _ => throw new Exception(s"Unknown direction $s.")

  case class Motion(direction: Direction, nrSteps: Int)
  case class Position(x: Int, y: Int):
    def move(d: Direction): Position = d match
      case Direction.Left => Position(x - 1, y)
      case Direction.Right => Position(x + 1, y)
      case Direction.Up => Position(x, y + 1)
      case Direction.Down => Position(x, y - 1)

  private def readSeriesOfMotions(lines: Iterator[String]): List[Motion] =
    def parseLine(line: String): Motion = line match
      case s"$direction $steps" => Motion(Direction(direction), steps.toInt)
      case _ => throw new Exception(s"Malformed input $line.")

    lines.map(parseLine).toList

  private def areNeighbours(p1: Position, p2: Position): Boolean =
    math.pow(p1.x - p2.x, 2) + math.pow(p1.y - p2.y, 2) <= 2

  private def followHead(head: Position, tail: Position): Position =
    if areNeighbours(head, tail) then tail
    else if head.x == tail.x + 2 && head.y == tail.y then Position(tail.x + 1, tail.y)
    else if head.x == tail.x - 2 && head.y == tail.y then Position(tail.x - 1, tail.y)
    else if head.y == tail.y + 2 && head.x == tail.x then Position(tail.x, tail.y + 1)
    else if head.y == tail.y - 2 && head.x == tail.x then Position(tail.x, tail.y - 1)
    else if head.x > tail.x && head.y > tail.y then Position(tail.x + 1, tail.y + 1)
    else if head.x > tail.x && head.y < tail.y then Position(tail.x + 1, tail.y - 1)
    else if head.x < tail.x && head.y < tail.y then Position(tail.x - 1, tail.y - 1)
    else Position(tail.x - 1, tail.y + 1)

  private def performMotions(moves: List[Motion]): Set[Position] =
    @tailrec
    def loop(visited: Set[Position], head: Position, tail: Position, ms: List[Motion]): Set[Position] = ms match
      case Nil => visited
      case (m @ Motion(direction, step)) :: mss =>
        if step == 0 then loop(visited, head, tail, mss)
        else
          val newHead: Position = head.move(direction)
          val newTail: Position = followHead(newHead, tail)
          loop(visited + newTail, newHead, newTail, m.copy(nrSteps = step - 1) :: mss)

    val start = Position(0, 0)
    loop(Set(start), start, start, moves)

  def calcNrVisitedPositions(moves: List[Motion]): Int =
    val visitedPositions: Set[Position] = performMotions(moves)
    visitedPositions.size

  def main(args: Array[String]): Unit =
    val lines: Iterator[String] = scala.io.Source.fromResource("input09.txt").getLines()
    val moves: List[Motion] = readSeriesOfMotions(lines)
    val result: Int = calcNrVisitedPositions(moves)
    println(result)

end Day09a
