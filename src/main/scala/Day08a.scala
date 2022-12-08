object Day08a:
  case class TreeGrid(nrRows: Int, nrColumns: Int, heights: Vector[Vector[Int]])

  private def readTrees(lines: Iterator[String]): TreeGrid =
    val heights: Vector[Vector[Int]] = lines.map(_.toVector.map(_.asDigit)).toVector
    val nrRows: Int = heights.length
    val nrColumns: Int = heights.head.length
    TreeGrid(nrRows = nrRows, nrColumns = nrColumns, heights = heights)

  def countNrVisibleTrees(grid: TreeGrid): Int =
    val TreeGrid(nrRows, nrColumns, treeHeights) = grid
    def isVisible(ix: Int, jy: Int): Boolean =
      val height: Int = treeHeights(ix)(jy)
      (0 until ix).forall(x => treeHeights(x)(jy) < height) ||
        (ix + 1 until nrColumns).forall(x => treeHeights(x)(jy) < height) ||
        (0 until jy).forall(y => treeHeights(ix)(y) < height) ||
        (jy + 1 until nrRows).forall(y => treeHeights(ix)(y) < height)

    (0 until nrColumns).foldLeft(0)((acc, x) => acc + (0 until nrRows).count(y => isVisible(x, y)))

  def main(args: Array[String]): Unit =
    val lines: Iterator[String] = scala.io.Source.fromResource("input08.txt").getLines()
    val grid: TreeGrid = readTrees(lines)
    val result: Int = countNrVisibleTrees(grid)
    println(result)

end Day08a
