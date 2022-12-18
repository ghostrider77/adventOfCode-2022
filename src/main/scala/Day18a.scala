object Day18a:
  case class Cube(x: Int, y: Int, z: Int):
    val faces: List[(Double, Double, Double)] =
      List((x + 0.5, y, z), (x - 0.5, y, z), (x, y + 0.5, z), (x, y - 0.5, z), (x, y, z + 0.5), (x, y, z - 0.5))

  def readInput(lines: Iterator[String]): List[Cube] =
    def parseLine(line: String): Cube = line.split(",").map(_.toInt).toList match
      case List(x, y, z) => Cube(x, y, z)
      case _ => throw Exception("Malformed input.")

    lines.map(parseLine).toList

  def countFaces(cubes: List[Cube]): Int =
    cubes
      .flatMap(_.faces)
      .groupMapReduce(identity)(_ => 1)(_ + _)
      .count{ case (_, occurrences) => occurrences == 1 }

  def main(args: Array[String]): Unit =
    val lines: Iterator[String] = scala.io.Source.fromResource("input18.txt").getLines()
    val cubes: List[Cube] = readInput(lines)
    val result: Int = countFaces(cubes)
    println(result)

end Day18a
