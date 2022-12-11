object Day11b:
  import scala.annotation.tailrec
  import scala.collection.mutable.{Queue => MutableQueue}
  type MonkeyId = Int

  class Monkey(startingItems: Seq[Long],
               val id: Int,
               private val operation: Long => Long,
               val modulus: Long,
               private val monkey1: MonkeyId,
               private val monkey2: MonkeyId):
    private val items: MutableQueue[Long] = MutableQueue.from(startingItems)

    override def toString: String = s"""Monkey $id, items: ${items.mkString(", ")}."""

    def receiveItem(item: Long): Unit =
      items.enqueue(item)

    def throwItem(m: Long): Option[(MonkeyId, Long)] =
      if items.isEmpty then None
      else
        val item: Long = items.dequeue()
        val updatedItem: Long = operation(item) % m
        if updatedItem % modulus == 0 then Some((monkey1, updatedItem))
        else Some((monkey2, updatedItem))

  end Monkey

  private def convertToLongList(s: String): List[Long] =
    s.split(", ").map(_.toLong).toList

  private def readMonkeys(lines: Iterator[String]): Vector[Monkey] =
    def removePrefix(s: String, prefix: String): String =
      s.stripLeading().stripPrefix(prefix)

    def parseMonkey(ls: Iterator[String]): Monkey =
      val id: Int = ls.next() match
        case s"Monkey $id:" => id.toInt
        case _ => throw new Exception("Malformed input.")
      val items: List[Long] = convertToLongList(removePrefix(ls.next(), "Starting items: "))
      val operation: Long => Long = removePrefix(ls.next(), "") match
        case "Operation: new = old * old" => x
          => x * x
        case "Operation: new = old + old" => x
          => 2 * x
        case s"Operation: new = old * $k" => x
          => k.toLong * x
        case s"Operation: new = old + $k" => x
          => k.toLong + x
        case _ => throw new Exception("Unknown operation.")
      val modulus: Long = removePrefix(ls.next(), "Test: divisible by ").toLong
      val nextMonkey: Int = removePrefix(ls.next(), "If true: throw to monkey ").toInt
      val alternativeMonkey: Int = removePrefix(ls.next(), "If false: throw to monkey ").toInt
      Monkey(items, id, operation, modulus, nextMonkey, alternativeMonkey)

    @tailrec
    def loop(acc: List[Monkey], ls: Iterator[String]): Vector[Monkey] =
      val (group, lss): (Iterator[String], Iterator[String]) = ls.span(_.nonEmpty)
      if group.isEmpty then acc.reverse.toVector
      else loop(parseMonkey(group) :: acc, lss.drop(1))

    loop(Nil, lines)

  private def calcLcm(xs: Seq[Long]): Long =
    @tailrec
    def calcGcd(a: Long, b: Long): Long = if (b == 0) a else calcGcd(b, a % b)

    xs.foldLeft(1L)(
      (acc, x) =>
        val gcd: Long = calcGcd(acc, x)
        (acc / gcd) * x
    )

  private def performRound(monkeys: Vector[Monkey], m: Long): List[Int] =
    def singleTurn(monkey: Monkey, monkeys: Vector[Monkey]): Int =
      @tailrec
      def loop(nrItemsThrown: Int): Int =
        monkey.throwItem(m) match
          case None => nrItemsThrown
          case Some((id, item)) =>
            monkeys(id).receiveItem(item)
            loop(nrItemsThrown + 1)

      loop(0)

    monkeys.foldLeft(List.empty[Int])((acc, monkey) => singleTurn(monkey, monkeys) :: acc).reverse

  def countMonkeyBusinessLevel(monkeys: Vector[Monkey], nrRounds: Int): Long =
    val lcm: Long = calcLcm(monkeys.map(_.modulus))

    @tailrec
    def loop(inspections: List[Int], k: Int): List[Int] =
      if k == nrRounds then inspections
      else loop(performRound(monkeys, lcm).zip(inspections).map(_ + _), k + 1)

    val nrInspections: List[Int] = loop(List.fill(monkeys.length)(0), 0)
    nrInspections.sorted(Ordering[Int].reverse).take(2).map(_.toLong).product

  def main(args: Array[String]): Unit =
    val lines: Iterator[String] = scala.io.Source.fromResource("input11.txt").getLines()
    val monkeys: Vector[Monkey] = readMonkeys(lines)
    val result: Long = countMonkeyBusinessLevel(monkeys, nrRounds = 10000)
    println(result)

end Day11b
