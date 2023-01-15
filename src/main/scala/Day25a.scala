object Day25a:
  import scala.annotation.tailrec
  import scala.math.Integral.Implicits._

  enum SnafuDigits:
    case MinusTwo, MinusOne, Zero, One, Two

    override def toString: String = this match
      case MinusTwo => "="
      case MinusOne => "-"
      case Zero => "0"
      case One => "1"
      case Two => "2"

    def toLong: Long = this match
      case MinusTwo => -2
      case MinusOne => -1
      case Zero => 0
      case One => 1
      case Two => 2

  object SnafuDigits:
    def apply(c: String): SnafuDigits = c match
      case "=" => MinusTwo
      case "-" => MinusOne
      case "0" => Zero
      case "1" => One
      case "2" => Two

  case class SnafuNumber(digits: List[SnafuDigits]):
    override def toString: String = digits.mkString

    def toLong: Long = digits.foldLeft(0L){ case (acc, d) => 5 * acc + d.toLong }

  object SnafuNumber:
    def apply(s: String): SnafuNumber =
      SnafuNumber(s.map(c => SnafuDigits(c.toString)).toList)

    def apply(n: Long): SnafuNumber =
      @tailrec
      def loop(acc: List[SnafuDigits], k: Long): SnafuNumber =
        if k == 0 then SnafuNumber(acc)
        else
          val (quotient, remainder): (Long, Long) = k /% 5
          remainder match
            case 3 => loop(SnafuDigits("=") :: acc, quotient + 1)
            case 4 => loop(SnafuDigits("-") :: acc, quotient + 1)
            case _ => loop(SnafuDigits(remainder.toString) :: acc, quotient)

      loop(Nil, n)

  def readInput(lines: Iterator[String]): List[SnafuNumber] =
    lines.map(SnafuNumber(_)).toList

  def calcSnafuSum(numbers: List[SnafuNumber]): SnafuNumber =
    SnafuNumber(numbers.map(_.toLong).sum)

  def main(args: Array[String]): Unit =
    val lines: Iterator[String] = scala.io.Source.fromResource("input25.txt").getLines()
    val numbers: List[SnafuNumber] = readInput(lines)
    val result: SnafuNumber = calcSnafuSum(numbers)
    println(result)

end Day25a
