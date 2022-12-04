object Day04a:
  final case class Section(start: Int, end: Int)

  object Section:
    def apply(s: String, e: String): Section =
      Section(s.toInt, e.toInt)

  private def readSectionAssigmnentPairs(lines: Iterator[String]): List[(Section, Section)] =
    def parseLine(line: String): (Section, Section) =
      val pattern = "(\\d+)-(\\d+),(\\d+)-(\\d+)".r
      line match
        case pattern(start1, end1, start2, end2) => (Section(start1, end1), Section(start2, end2))
        case _ => throw new Exception(s"Malformed input: $line.")

    lines.map(parseLine).toList

  private def isFullyContained(section1: Section, section2: Section): Boolean =
    section1.start >= section2.start && section1.end <= section2.end

  def calcNumberOfFullyContainedPairs(sectionPairs: List[(Section, Section)]): Int =
    sectionPairs.foldLeft(0){ case (acc, (section1, section2)) =>
      if isFullyContained(section1, section2) || isFullyContained(section2, section1) then acc + 1 else acc
    }

  def main(args: Array[String]): Unit =
    val lines: Iterator[String] = scala.io.Source.fromResource("input04.txt").getLines()
    val sectionPairs = readSectionAssigmnentPairs(lines)
    val result: Int = calcNumberOfFullyContainedPairs(sectionPairs)
    println(result)

end Day04a

