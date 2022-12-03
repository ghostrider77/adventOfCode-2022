object Day03b:
  private val Priorities: Map[Char, Int] =
    ('a' to 'z').zip(Iterator.from(1)).toMap ++ ('A' to 'Z').zip(Iterator.from(27)).toMap

  private def findSharedItems(group: List[String]): Set[Char] = group match
    case Nil => Set.empty[Char]
    case first :: rest => rest.foldLeft(first.toSet)((acc, rucksack) => acc.intersect(rucksack.toSet))

  def calcSumOfPriorities(rucksackGroups: List[List[String]]): Int =
    rucksackGroups.foldLeft(0)((acc, group) => acc + findSharedItems(group).map(Priorities).sum)

  def main(args: Array[String]): Unit =
    val lines: Iterator[String] = scala.io.Source.fromResource("input03.txt").getLines()
    val groups: List[List[String]] = lines.grouped(3).map(_.toList).toList
    val result: Int = calcSumOfPriorities(groups)
    println(result)

end Day03b
