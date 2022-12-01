object Day01a:
  import scala.annotation.tailrec

  private def readCaloriesCarriedByElves(lines: Iterator[String]): List[List[Int]] =
    @tailrec
    def loop(acc: List[List[Int]], calories: Iterator[String]): List[List[Int]] =
      if calories.isEmpty then acc.reverse
      else
        val (currentElf, rest): (Iterator[String], Iterator[String]) = calories.span(_ != "")
        loop(currentElf.map(_.toInt).toList :: acc, rest.drop(1))

    loop(Nil, lines)

  def getMaximalCalories(caloriesCarried: List[List[Int]]): Int =
    caloriesCarried.maxBy(_.sum).sum

  def main(args: Array[String]): Unit =
    val lines: Iterator[String] = scala.io.Source.fromResource("input01.txt").getLines()
    val calories: List[List[Int]] = readCaloriesCarriedByElves(lines)
    val result: Int = getMaximalCalories(calories)
    println(result)

end Day01a
