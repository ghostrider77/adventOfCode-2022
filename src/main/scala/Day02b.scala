object Day02b:
  import scala.math.Ordering.Implicits.infixOrderingOps

  enum Shape:
    case Rock, Paper, Scissors

    def score: Int = this match
      case Rock => 1
      case Paper => 2
      case Scissors => 3

  object Shape:
    def apply(s: String): Shape = s match
      case "A" => Rock
      case "B" => Paper
      case "C" => Scissors
      case _ => throw new Exception(s"Unknown shape $s.")

    given Ordering[Shape] with
      def compare(s1: Shape, s2: Shape): Int = (s1, s2) match
        case (Rock, Paper) => -1
        case (Rock, Scissors) => 1
        case (Paper, Rock) => 1
        case (Paper, Scissors) => -1
        case (Scissors, Rock) => -1
        case (Scissors, Paper) => 1
        case _ => 0

  end Shape

  enum Outcome:
    case Win, Draw, Lose

  object Outcome:
    def apply(s: String): Outcome = s match
      case "X" => Win
      case "Y" => Draw
      case "Z" => Lose
      case _ => throw new Exception(s"Unknown outcome of a round $s.")

  final case class Round(opponentsShape: Shape, myShape: Shape):
    private val outcome: Int =
      if myShape > opponentsShape then 6
      else if myShape == opponentsShape then 3
      else 0

    def score: Int = outcome + myShape.score

  def getRequestedShape(opponentsShape: Shape, outcome: Outcome)(using ord: Ordering[Shape]): Shape =
    val compare: (Shape, Shape) => Boolean = outcome match
      case Outcome.Win => ord.lt
      case Outcome.Draw => ord.equiv
      case Outcome.Lose => ord.gt

    Shape.values.find(compare(_, opponentsShape)) match
      case Some(shape) => shape
      case None => throw new Exception(s"Impossible to find required shape against $opponentsShape.")

  private def readRounds(lines: Iterator[String]): List[Round] =
    def createRound(line: String): Round = line match
      case s"${shape1} ${expectedOutcome}" =>
        val opponentsShape = Shape(shape1)
        val outcome = Outcome(expectedOutcome)
        Round(opponentsShape, getRequestedShape(opponentsShape, outcome))
      case _ => throw new Exception(s"Malformed input $line")

    lines.map(createRound).toList

  def calcTotalScore(rounds: List[Round]): Int =
    rounds.foldLeft(0)((acc, round) => acc + round.score)

  def main(args: Array[String]): Unit =
    val lines: Iterator[String] = scala.io.Source.fromResource("input02.txt").getLines()
    val rounds: List[Round] = readRounds(lines)
    val result: Int = calcTotalScore(rounds)
    println(result)

end Day02b
