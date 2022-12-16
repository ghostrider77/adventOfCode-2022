object Day15b:
  import scala.annotation.tailrec

  case class Cell(x: Int, y: Int)
  object Cell:
    def apply(x: String, y: String): Cell =
      Cell(x.toInt, y.toInt)

  case class Interval(a: Int, b: Int):
    val nrPoints: Int = b - a + 1

    def union(that: Interval): Option[Interval] =
      val Interval(x, y) = that
      if y <= b then Some(this)
      else if x <= b then Some(Interval(a, y))
      else None

  case class Sensor(center: Cell, closestBeacon: Cell):
    val distance: Int = math.abs(center.x - closestBeacon.x) + math.abs(center.y - closestBeacon.y)

    def horizontalSection(y: Int): Option[Interval] =
      val diff: Int = math.abs(center.y - y)
      if diff > distance then None
      else
        val d: Int = distance - diff
        val xMin: Int = center.x - d
        val xMax: Int = center.x + d
        Some(Interval(xMin, xMax))

  def parseData(lines: Iterator[String]): List[Sensor] =
    def parseLine(line: String): Sensor = line match
      case s"Sensor at x=$sx, y=$sy: closest beacon is at x=$bx, y=$by" =>
        Sensor(center = Cell(sx, sy), closestBeacon = Cell(bx, by))
      case _ => throw Exception("Malformed input.")

    lines.map(parseLine).toList

  private def calcUndetectedBeacon(sensors: List[Sensor], y: Int, limit: Int): Option[Cell] =
    @tailrec
    def loop(acc: List[Interval], current: Interval, xs: List[Interval]): List[Interval] = xs match
      case Nil => (current :: acc).reverse
      case x :: xss =>
        current.union(x) match
          case Some(updatedInterval) => loop(acc, updatedInterval, xss)
          case None => loop(current :: acc, x, xss)

    val intervals: List[Interval] =
      sensors
        .flatMap(_.horizontalSection(y))
        .groupMapReduce(_.a)(identity){ case (Interval(a, b1), Interval(_, b2)) => Interval(a, math.max(b1, b2)) }
        .values
        .toList
        .sortBy(_.a)

    val mergedIntervals: List[Interval] = loop(Nil, intervals.head, intervals.tail)
    val beaconCoordinate: Option[Int] =
      mergedIntervals
      .sliding(2)
      .collect{ case List(Interval(_, b), Interval(a, _)) if a - b == 2 && b + 1 >= 0 && b + 1 <= limit => b + 1 }
      .nextOption()
    beaconCoordinate.map(x => Cell(x, y))

  def findUndetectedBeacon(sensors: List[Sensor], limit: Int): Long =
    val Cell(x, y) = (0 to limit).flatMap(calcUndetectedBeacon(sensors, _, limit)).head
    x.toLong * limit + y

  def main(args: Array[String]): Unit =
    val lines: Iterator[String] = scala.io.Source.fromResource("input15.txt").getLines()
    val sensors: List[Sensor] = parseData(lines)
    val limit: Int = 4000000
    val result: Long = findUndetectedBeacon(sensors, limit)
    println(result)

end Day15b
