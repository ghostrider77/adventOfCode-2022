object Day01b:
  import scala.annotation.tailrec

  private def readCaloriesCarriedByElves(lines: Iterator[String]): List[List[Int]] =
    @tailrec
    def loop(acc: List[List[Int]], calories: Iterator[String]): List[List[Int]] =
      if calories.isEmpty then acc.reverse
      else
        val (currentElf, rest): (Iterator[String], Iterator[String]) = calories.span(_ != "")
        loop(currentElf.map(_.toInt).toList :: acc, rest.drop(1))

    loop(Nil, lines)

  def getTopKCalories(caloriesCarried: List[List[Int]], k: Int): Int =
    caloriesCarried.map(_.sum).sorted(Ordering[Int].reverse).take(k).sum

  def main(args: Array[String]): Unit =
    val lines: Iterator[String] = scala.io.Source.fromResource("input01.txt").getLines()
    val calories: List[List[Int]] = readCaloriesCarriedByElves(lines)
    val result: Int = getTopKCalories(calories, k = 3)
    println(result)

end Day01b
