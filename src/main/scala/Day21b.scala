object Day21b:
  import scala.annotation.tailrec
  private val Root: String = "root"

  case class Operation(op: String):
    def apply(value1: Long, value2: Long): Long = op match
      case "+" => value1 + value2
      case "-" => value1 - value2
      case "*" => value1 * value2
      case "/" => value1 / value2
      case _ => throw Exception(s"Unknown operation $op.")

    def inverse: Operation = op match
      case "+" => Operation("-")
      case "-" => Operation("+")
      case "*" => Operation("/")
      case "/" => Operation("*")
      case _ => throw Exception(s"No inverse operation for $op.")

  case class DelayedOperations(ops: List[Long => Long])

  sealed trait Node
  case class Leaf(name: String, value: Either[DelayedOperations, Long]) extends Node
  case class Branch(name: String, op: Operation, leftChild: String, rightChild: String) extends Node

  case class Graph(nodes: Map[String, Node]):
    private val leaves: Map[String, Leaf] = nodes.collect{ case (name, node: Leaf) => name -> node }

    private def getRipeNode(nodeName: String): Option[(Branch, (Leaf, Leaf))] =
      nodes.get(nodeName) match
        case Some(b @ Branch(_, _, leftChildName, rightChildName)) =>
          for
            leftChild <- leaves.get(leftChildName)
            rightChild <- leaves.get(rightChildName)
          yield (b, (leftChild, rightChild))
        case _ => None

    def ripeNodes: Set[(Branch, (Leaf, Leaf))] =
      (for
        name <- nodes.keysIterator
        nodeWithChildren <- getRipeNode(name)
      yield nodeWithChildren).toSet

  def readInput(lines: Iterator[String]): Graph =
    @tailrec
    def loop(acc: Map[String, Node]): Graph = lines.nextOption() match
      case None => Graph(acc)
      case Some(line) => line match
        case s"$name: $left $op $right" => loop(acc + (name -> Branch(name, Operation(op), left, right)))
        case s"$name: $value" =>
          if name == "humn" then loop(acc + (name -> Leaf(name, Left(DelayedOperations(Nil)))))
          else loop(acc + (name -> Leaf(name, Right(value.toLong))))
        case _ => loop(acc)

    loop(Map())

  private def performDelayedOperations(value1: Either[DelayedOperations, Long],
                                       value2: Either[DelayedOperations, Long]): Long = (value1, value2) match
    case (Left(DelayedOperations(fs)), Right(value)) => fs.foldLeft(value)((acc, f) => f(acc))
    case (Right(value), Left(DelayedOperations(fs))) => fs.foldLeft(value)((acc, f) => f(acc))
    case _ => throw Exception("Exactly one value should be already computed.")

  private def calcNewLeaves(ripeNodes: Set[(Branch, (Leaf, Leaf))]): Map[String, Node] =
    ripeNodes.map {
      case (Branch(name, op, _, _), (Leaf(_, value1), Leaf(_, value2))) =>
        (value1, value2) match
          case (Right(v1), Right(v2)) =>
            name -> Leaf(name, Right(op(v1, v2)))
          case (Left(DelayedOperations(ops)), Right(v2)) =>
            name -> Leaf(name, Left(DelayedOperations((y => op.inverse(y, v2)) :: ops)))
          case (Right(v1), Left(DelayedOperations(ops))) =>
            if op.op == "+" || op.op == "*"
            then name -> Leaf(name, Left(DelayedOperations((y => op.inverse(y, v1)) :: ops)))
            else name -> Leaf(name, Left(DelayedOperations((y => op(v1, y)) :: ops)))
          case _ => throw Exception("Both children cannot have delayed operations.")
    }.toMap

  def squeezeGraph(graph: Graph): Long =
    @tailrec
    def loop(currentGraph: Graph): Long =
      val ripeNodes: Set[(Branch, (Leaf, Leaf))] = currentGraph.ripeNodes
      ripeNodes.find{ case (Branch(name, _, _, _), _) => name == Root } match
        case Some((_, (Leaf(_, value1), Leaf(_, value2)))) => performDelayedOperations(value1, value2)
        case None =>
          val newLeaves: Map[String, Node] = calcNewLeaves(ripeNodes)
          val oldLeafNames: Set[String] =
            ripeNodes.flatMap{ case (_, (Leaf(name1, _), Leaf(name2, _))) => List(name1, name2) }
          val nodes: Map[String, Node] =
            currentGraph
              .nodes
              .filter{ case (name, _) => !oldLeafNames.contains(name) }
          loop(Graph(nodes ++ newLeaves))
    loop(graph)

  def main(args: Array[String]): Unit =
    val lines: Iterator[String] = scala.io.Source.fromResource("input21.txt").getLines()
    val graph: Graph = readInput(lines)
    val result: Long = squeezeGraph(graph)
    println(result)

end Day21b
