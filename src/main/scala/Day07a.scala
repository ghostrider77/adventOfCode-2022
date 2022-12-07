object Day07a:
  import scala.annotation.tailrec

  case class Path(path: String):
    private val root: String = ""
    private val parts: Vector[String] = path.split("/").toVector

    val name: String = parts.lastOption match
      case None => root
      case Some(last) => last

    def parent: Path = Path(parts.dropRight(1).mkString("/"))

    def /(name: String): Path = Path(s"$path/$name")
  end Path

  sealed trait FileSystem
  case class File(path: Path, size: Int) extends FileSystem
  case class Directory(path: Path, subdirectories: Set[Path], files: Set[File]) extends FileSystem

  private def findPathOfSubdirectory(subdirectories: Set[Path], foldername: String): Path =
    subdirectories.find(_.name == foldername) match
      case None => throw new Exception("Folder is not found!")
      case Some(subfolderPath) => subfolderPath

  def parseFileSystem(lines: Iterator[String]): Map[Path, Directory] =
    @tailrec
    def loop(tree: Map[Path, Directory], path: Path): Map[Path, Directory] =
      val currentDirectory @ Directory(_, subdirectories, files) = tree(path)
      lines.nextOption() match
        case None => tree
        case Some(line) => line match
          case s"$$ cd /" => loop(tree, Path(""))
          case s"$$ cd .." => loop(tree, path.parent)
          case s"$$ cd $foldername" =>
            loop(tree, findPathOfSubdirectory(subdirectories, foldername))
          case s"$$ ls" => loop(tree, path)
          case s"dir $dirname" =>
            val subfolderPath: Path = path / dirname
            val updatedTree: Map[Path, Directory] =
              if !tree.contains(subfolderPath) then tree
                .updated(path, currentDirectory.copy(subdirectories = subdirectories + (path / dirname)))
                .updated(subfolderPath, Directory(subfolderPath, Set.empty[Path], Set.empty[File]))
              else tree
            loop(updatedTree, path)
          case s"$size $name" =>
            val updatedTree: Map[Path, Directory] =
              tree.updated(path, currentDirectory.copy(files = files + File(path / name, size.toInt)))
            loop(updatedTree, path)
          case _ => throw new Exception(s"Unknown terminal output $line.")

    val root: Directory = Directory(path = Path(""), subdirectories = Set.empty[Path], files = Set.empty[File])
    loop(Map(root.path -> root), root.path)

  private def totalSizeOfDirectory(tree: Map[Path, Directory], path: Path): Int = tree.get(path) match
      case None => 0
      case Some(Directory(_, subdirectories, files)) =>
        files.map(_.size).sum + subdirectories.map(totalSizeOfDirectory(tree, _)).sum

  def calcTotalSizeOfSmallDirectories(tree: Map[Path, Directory], limit: Int): Int =
    tree.keysIterator.map(totalSizeOfDirectory(tree, _)).filter(_ <= limit).sum

  def main(args: Array[String]): Unit =
    val lines: Iterator[String] = scala.io.Source.fromResource("input07.txt").getLines()
    val limit: Int = 100000
    val directoryStructure: Map[Path, Directory] = parseFileSystem(lines)
    val result: Int = calcTotalSizeOfSmallDirectories(directoryStructure, limit)
    println(result)

end Day07a
