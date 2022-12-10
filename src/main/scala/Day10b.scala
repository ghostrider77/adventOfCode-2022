object Day10b:
  import scala.annotation.tailrec

  sealed trait Program
  case object Noop extends Program
  case class AddX(value: Int, duration: Int = 2) extends Program

  enum Pixel:
    case Dark, Lit

    override def toString: String = this match
      case Dark => "."
      case Lit => "#"

  def readInstructions(lines: Iterator[String]): List[Program] =
    def parseLine(line: String): Program = line match
      case "noop" => Noop
      case s"addx $value" => AddX(value.toInt)

    lines.map(parseLine).toList

  def calcScreenContent(instructions: List[Program]): List[Pixel] =
    @tailrec
    def loop(acc: List[Pixel], register: Int, cycleId: Int, ps: List[Program]): List[Pixel] = ps match
      case Nil => acc.reverse
      case program :: pss =>
        val horizontalPosition: Int = register % 40
        val pixel: Pixel = if math.abs(horizontalPosition - cycleId % 40) <= 1 then Pixel.Lit else Pixel.Dark
        val crt: List[Pixel] = pixel :: acc
        program match
          case Noop => loop(crt, register, cycleId + 1, pss)
          case AddX(value, duration) =>
            if duration == 2 then loop(crt, register, cycleId + 1, AddX(value, 1) :: pss)
            else loop(crt, register + value, cycleId + 1, pss)

    loop(Nil, register = 1, cycleId = 0, ps = instructions)

  def main(args: Array[String]): Unit =
    val lines: Iterator[String] = scala.io.Source.fromResource("input10.txt").getLines()
    val instructions: List[Program] = readInstructions(lines)
    val result: List[Pixel] = calcScreenContent(instructions)
    result.grouped(40).foreach(row => println(row.mkString))

end Day10b

