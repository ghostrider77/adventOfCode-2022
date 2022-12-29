object Day20a:
  import scala.collection.mutable.ArrayDeque as Deque
  private type Index = Int

  private def readInput(lines: Iterator[String]): List[Int] =
    lines.map(_.toInt).toList

  def mixSequence(sequence: List[Int], givenIndices: List[Index]): Int =
    val n: Int = sequence.length
    val deque: Deque[(Int, Index)] = Deque.from(sequence.zipWithIndex)
    def getCurrentIndex(originalIx: Index): Index = deque.indexWhere{ case (_, ix) => ix == originalIx }

    (0 until n).foreach{ k =>
      val currentIx: Index = getCurrentIndex(k)
      val (value, _): (Int, Index) = deque.remove(currentIx)
      val jy: Index = (currentIx + value) % (n - 1)
      val nextIx: Index = if jy < 0 then jy + n - 1 else jy
      deque.insert(nextIx, (value, k))
    }

    val zeroIndex: Index = deque.indexWhere{ case (value, _) => value == 0 }
    givenIndices.map(ix => deque((ix + zeroIndex) % n)).map{ case (value, _) => value }.sum

  def main(args: Array[String]): Unit =
    val lines: Iterator[String] = scala.io.Source.fromResource("input20.txt").getLines()
    val sequence: List[Int] = readInput(lines)
    val indices: List[Int] = List(1000, 2000, 3000)
    val result: Int = mixSequence(sequence, indices)
    println(result)

end Day20a
