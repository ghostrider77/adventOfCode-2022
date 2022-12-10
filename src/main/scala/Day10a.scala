object Day10a:
  import scala.annotation.tailrec

  sealed trait Program
  case object Noop extends Program
  case class AddX(value: Int, duration: Int = 2) extends Program

  def readInstructions(lines: Iterator[String]): List[Program] =
    def parseLine(line: String): Program = line match
      case "noop" => Noop
      case s"addx $value" => AddX(value.toInt)

    lines.map(parseLine).toList

  def calcSumOfSignalStrengths(instructions: List[Program]): Int =
    @tailrec
    def loop(acc: Int, register: Int, cycleId: Int, ps: List[Program]): Int = ps match
      case Nil => acc
      case program :: pss =>
        val strength: Int = if cycleId % 40 == 20 then cycleId * register + acc else acc
        program match
          case Noop => loop(strength, register, cycleId + 1, pss)
          case AddX(value, duration) =>
            if duration == 2 then loop(strength, register, cycleId + 1, AddX(value, 1) :: pss)
            else loop(strength, register + value, cycleId + 1, pss)

    loop(0, register = 1, cycleId = 1, ps = instructions)

  def main(args: Array[String]): Unit =
    val lines: Iterator[String] = scala.io.Source.fromResource("input10.txt").getLines()
    val instructions: List[Program] = readInstructions(lines)
    val result: Int = calcSumOfSignalStrengths(instructions)
    println(result)

end Day10a
