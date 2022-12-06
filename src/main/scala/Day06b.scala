object Day06b:
  def findStartOfMessageMarker(stream: String, packetSize: Int): Int =
    def areDifferentCharacters(s: String): Boolean =
      s.toSet.size == packetSize

    stream.sliding(packetSize).zipWithIndex.find((packet, _) => areDifferentCharacters(packet)) match
      case Some((_, ix)) => ix + packetSize
      case None => throw new Exception("Start marker was not found.")

  def main(args: Array[String]): Unit =
    val lines: Iterator[String] = scala.io.Source.fromResource("input06.txt").getLines()
    val datastream: String = lines.next()
    val packetSize: Int = 14
    val result: Int = findStartOfMessageMarker(datastream, packetSize)
    println(result)

end Day06b
