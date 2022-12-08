object Day08b:
  case class TreeGrid(nrRows: Int, nrColumns: Int, heights: Vector[Vector[Int]])

  private def readTrees(lines: Iterator[String]): TreeGrid =
    val heights: Vector[Vector[Int]] = lines.map(_.toVector.map(_.asDigit)).toVector
    val nrRows: Int = heights.length
    val nrColumns: Int = heights.head.length
    TreeGrid(nrRows = nrRows, nrColumns = nrColumns, heights = heights)

  def getHighestScenicScore(grid: TreeGrid): Int =
    val TreeGrid(nrRows, nrColumns, treeHeights) = grid
    def calcScenicScore(ix: Int, jy: Int): Int =
      val height: Int = treeHeights(ix)(jy)
      val up: Int = (jy - 1 to 0 by -1).find(y => treeHeights(ix)(y) >= height).map(k => jy - k).getOrElse(jy)
      val down: Int =
        (jy + 1 until nrRows).find(y => treeHeights(ix)(y) >= height).map(k => k - jy).getOrElse(nrRows - jy - 1)
      val left: Int = (ix - 1 to 0 by -1).find(x => treeHeights(x)(jy) >= height).map(k => ix - k).getOrElse(ix)
      val right: Int =
        (ix + 1 until nrColumns).find(x => treeHeights(x)(jy) >= height).map(k => k - ix).getOrElse(nrColumns - 1 - ix)
      up * down * left * right

    val coords: Iterator[(Int, Int)] = (0 until nrColumns).iterator.flatMap(x => (0 until nrRows).map(y => (x, y)))
    coords.foldLeft(0){ case (acc, (x, y)) => math.max(acc, calcScenicScore(x, y)) }

  def main(args: Array[String]): Unit =
    val lines: Iterator[String] = scala.io.Source.fromResource("input08.txt").getLines()
    val grid: TreeGrid = readTrees(lines)
    val result: Int = getHighestScenicScore(grid)
    println(result)

end Day08b
