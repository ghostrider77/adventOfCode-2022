object Day03a:
  private val Priorities: Map[Char, Int] =
    ('a' to 'z').zip(Iterator.from(1)).toMap ++ ('A' to 'Z').zip(Iterator.from(27)).toMap

  private def findSharedItems(rucksack: String): Set[Char] =
    val k: Int = rucksack.length / 2
    val (first, second): (String, String) = rucksack.splitAt(k)
    first.intersect(second).toSet

  def calcSumOfPriorities(rucksacks: List[String]): Int =
    rucksacks.foldLeft(0)((acc, rucksack) => acc + findSharedItems(rucksack).map(Priorities).sum)

  def main(args: Array[String]): Unit =
    val lines: Iterator[String] = scala.io.Source.fromResource("input03.txt").getLines()
    val result: Int = calcSumOfPriorities(lines.toList)
    println(result)

end Day03a
