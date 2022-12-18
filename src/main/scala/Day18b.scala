object Day18b:
  import scala.annotation.tailrec
  import scala.collection.mutable.Queue as MutableQueue

  type Face = (Double, Double, Double)

  case class EnclosingArea(xMin: Int, yMin: Int, zMin: Int, xMax: Int, yMax: Int, zMax: Int):
    def contains(cube: Cube): Boolean =
      val Cube(x, y, z) = cube
      xMin <= x && x <= xMax && yMin <= y && y <= yMax && zMin <= z && z <= zMax

    val corner: Cube = Cube(xMin, yMin, zMin)

  case class Cube(x: Int, y: Int, z: Int):
    val faces: List[Face] =
      List((x + 0.5, y, z), (x - 0.5, y, z), (x, y + 0.5, z), (x, y - 0.5, z), (x, y, z + 0.5), (x, y, z - 0.5))

    def neighbours: List[Cube] =
      List(
        Cube(x + 1, y, z),
        Cube(x - 1, y, z),
        Cube(x, y + 1, z),
        Cube(x, y - 1, z),
        Cube(x, y, z + 1),
        Cube(x, y, z - 1)
      )

  def readInput(lines: Iterator[String]): Set[Cube] =
    def parseLine(line: String): Cube = line.split(",").map(_.toInt).toList match
      case List(x, y, z) => Cube(x, y, z)
      case _ => throw Exception("Malformed input.")

    lines.map(parseLine).toSet

  private def calcEnclosingSpace(cubes: Set[Cube]): EnclosingArea =
    val xMin: Int = cubes.map(_.x).min
    val yMin: Int = cubes.map(_.y).min
    val zMin: Int = cubes.map(_.z).min
    val xMax: Int = cubes.map(_.x).max
    val yMax: Int = cubes.map(_.y).max
    val zMax: Int = cubes.map(_.z).max
    EnclosingArea(xMin - 1, yMin - 1, zMin - 1, xMax + 1, yMax + 1, zMax + 1)

  private def getFreeFaces(cubes: Set[Cube]): Set[Face] =
    cubes
      .flatMap(_.faces)
      .groupMapReduce(identity)(_ => 1)(_ + _)
      .filter { case (_, occurrences) => occurrences == 1 }
      .keySet

  private def getExteriorCubes(cubes: Set[Cube]): Set[Cube] =
    val searchSpace: EnclosingArea = calcEnclosingSpace(cubes)
    val queue: MutableQueue[Cube] = MutableQueue(searchSpace.corner)

    @tailrec
    def loop(exterior: Set[Cube]): Set[Cube] =
      if queue.isEmpty then exterior
      else
        val cube: Cube = queue.dequeue()
        val exteriorNeighbours: List[Cube] =
          cube.neighbours.filter(c => searchSpace.contains(c) && !cubes.contains(c) && !exterior.contains(c))
        queue.enqueueAll(exteriorNeighbours)
        loop(exterior ++ exteriorNeighbours)

    loop(Set(searchSpace.corner))

  def calcNrCubeSidesFacingOutside(cubes: Set[Cube]): Int =
    val exteriorCubes: Set[Cube] = getExteriorCubes(cubes)
    val freeFaces: Set[Face] = getFreeFaces(cubes)
    exteriorCubes
      .flatMap(_.faces)
      .intersect(freeFaces)
      .size

  def main(args: Array[String]): Unit =
    val lines: Iterator[String] = scala.io.Source.fromResource("input18.txt").getLines()
    val cubes: Set[Cube] = readInput(lines)
    val result: Int = calcNrCubeSidesFacingOutside(cubes)
    println(result)

end Day18b
