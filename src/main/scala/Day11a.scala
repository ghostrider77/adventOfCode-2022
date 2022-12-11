object Day11a:
  import scala.annotation.tailrec
  import scala.collection.mutable.{Queue => MutableQueue}
  type MonkeyId = Int

  class Monkey(startingItems: Seq[Int],
               val id: Int,
               private val operation: Int => Int,
               private val modulus: Int,
               private val monkey1: MonkeyId,
               private val monkey2: MonkeyId):
    private val items: MutableQueue[Int] = MutableQueue.from(startingItems)

    override def toString: String = s"""Monkey $id, items: ${items.mkString(", ")}."""

    def receiveItem(item: Int): Unit =
      items.enqueue(item)

    def throwItem(): Option[(MonkeyId, Int)] =
      if items.isEmpty then None
      else
        val item: Int = items.dequeue()
        val updatedItem: Int = operation(item) / 3
        if updatedItem % modulus == 0 then Some((monkey1, updatedItem))
        else Some((monkey2, updatedItem))

  end Monkey

  private def convertToIntList(s: String): List[Int] =
    s.split(", ").map(_.toInt).toList

  private def readMonkeys(lines: Iterator[String]): Vector[Monkey] =
    def removePrefix(s: String, prefix: String): String =
      s.stripLeading().stripPrefix(prefix)

    def parseMonkey(ls: Iterator[String]): Monkey =
      val id: Int = ls.next() match
        case s"Monkey $id:" => id.toInt
        case _ => throw new Exception("Malformed input.")
      val items: List[Int] = convertToIntList(removePrefix(ls.next(), "Starting items: "))
      val operation: Int => Int = removePrefix(ls.next(), "") match
        case "Operation: new = old * old" => x => x * x
        case "Operation: new = old + old" => x => 2 * x
        case s"Operation: new = old * $k" => x => k.toInt * x
        case s"Operation: new = old + $k" => x => k.toInt + x
        case _ => throw new Exception("Unknown operation.")
      val modulus: Int = removePrefix(ls.next(), "Test: divisible by ").toInt
      val nextMonkey: Int = removePrefix(ls.next(), "If true: throw to monkey ").toInt
      val alternativeMonkey: Int = removePrefix(ls.next(), "If false: throw to monkey ").toInt
      Monkey(items, id, operation, modulus, nextMonkey, alternativeMonkey)

    @tailrec
    def loop(acc: List[Monkey], ls: Iterator[String]): Vector[Monkey] =
      val (group, lss): (Iterator[String], Iterator[String]) = ls.span(_.nonEmpty)
      if group.isEmpty then acc.reverse.toVector
      else loop(parseMonkey(group) :: acc, lss.drop(1))

    loop(Nil, lines)

  private def singleTurn(monkey: Monkey, monkeys: Vector[Monkey]): Int =
    @tailrec
    def loop(nrItemsThrown: Int): Int =
      monkey.throwItem() match
        case None => nrItemsThrown
        case Some((id, item)) =>
          monkeys(id).receiveItem(item)
          loop(nrItemsThrown + 1)

    loop(0)

  private def performRound(monkeys: Vector[Monkey]): List[Int] =
    monkeys.foldLeft(List.empty[Int])((acc, monkey) => singleTurn(monkey, monkeys) :: acc).reverse

  def countMonkeyBusinessLevel(monkeys: Vector[Monkey], nrRounds: Int): Int =
    @tailrec
    def loop(inspections: List[Int], k: Int): List[Int] =
      if k == nrRounds then inspections
      else loop(performRound(monkeys).zip(inspections).map(_ + _), k + 1)

    val nrInspections: List[Int] = loop(List.fill(monkeys.length)(0), 0)
    nrInspections.sorted(Ordering[Int].reverse).take(2).product

  def main(args: Array[String]): Unit =
    val lines: Iterator[String] = scala.io.Source.fromResource("input11.txt").getLines()
    val monkeys: Vector[Monkey] = readMonkeys(lines)
    val result: Int = countMonkeyBusinessLevel(monkeys, nrRounds = 20)
    println(result)

end Day11a
