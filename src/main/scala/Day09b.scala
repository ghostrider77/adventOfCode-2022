object Day09b:
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

  private def oneCompleteStep(direction: Direction, rope: List[Position]): List[Position] =
    val newHead: Position = rope.head.move(direction)
    @tailrec
    def loop(acc: List[Position], tail: List[Position]): List[Position] = tail match
      case Nil => acc.reverse
      case tail :: rest => loop(followHead(acc.head, tail) :: acc, rest)

    loop(List(newHead), rope.tail)

  private def performMotions(moves: List[Motion], size: Int): Set[Position] =
    @tailrec
    def loop(visited: Set[Position], rope: List[Position], ms: List[Motion]): Set[Position] = ms match
      case Nil => visited
      case (m @ Motion(direction, step)) :: mss =>
        if step == 0 then loop(visited, rope, mss)
        else
          val newRope: List[Position] = oneCompleteStep(direction, rope)
          loop(visited + newRope.last, newRope, m.copy(nrSteps = step - 1) :: mss)

    val start = Position(0, 0)
    loop(Set(start), List.fill(size)(start), moves)

  def calcNrVisitedPositions(moves: List[Motion], size: Int): Int =
    val visitedPositions: Set[Position] = performMotions(moves, size)
    visitedPositions.size

  def main(args: Array[String]): Unit =
    val lines: Iterator[String] = scala.io.Source.fromResource("input09.txt").getLines()
    val moves: List[Motion] = readSeriesOfMotions(lines)
    val size: Int = 10
    val result: Int = calcNrVisitedPositions(moves, size)
    println(result)

end Day09b
