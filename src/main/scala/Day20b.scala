object Day20b:
  import scala.collection.mutable.ArrayDeque as Deque
  private type Index = Int

  val DecriptionKey: Long = 811589153L

  private def readInput(lines: Iterator[String]): List[Long] =
    lines.map(_.toLong * DecriptionKey).toList

  def mixSequence(sequence: List[Long], nrRounds: Int, givenIndices: List[Index]): Long =
    val n: Int = sequence.length
    val deque: Deque[(Long, Index)] = Deque.from(sequence.zipWithIndex)
    def getCurrentIndex(originalIx: Index): Index = deque.indexWhere{ case (_, ix) => ix == originalIx }

    (0 until nrRounds).foreach { _ =>
      (0 until n).foreach { k =>
        val currentIx: Index = getCurrentIndex(k)
        val (value, _): (Long, Index) = deque.remove(currentIx)
        val jy: Index = ((currentIx + value) % (n - 1)).toInt
        val nextIx: Index = if jy < 0 then jy + n - 1 else jy
        deque.insert(nextIx, (value, k))
      }
    }

    val zeroIndex: Index = deque.indexWhere{ case (value, _) => value == 0 }
    givenIndices.map(ix => deque((ix + zeroIndex) % n)).map{ case (value, _) => value }.sum

  def main(args: Array[String]): Unit =
    val lines: Iterator[String] = scala.io.Source.fromResource("input20.txt").getLines()
    val sequence: List[Long] = readInput(lines)
    val nrRounds: Int = 10
    val indices: List[Int] = List(1000, 2000, 3000)
    val result: Long = mixSequence(sequence, nrRounds, indices)
    println(result)

end Day20b
