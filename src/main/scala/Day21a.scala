object Day21a:
  import scala.annotation.tailrec
  private val Root: String = "root"

  sealed trait Node
  case class Leaf(name: String, value: Long) extends Node
  case class Branch(name: String, op: (Long, Long) => Long, leftChild: String, rightChild: String) extends Node

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
    def getOperator(s: String): (Long, Long) => Long = s match
      case "+" => (x, y) => x + y
      case "-" => (x, y) => x - y
      case "*" => (x, y) => x * y
      case "/" => (x, y) => x / y
      case _ => throw Exception(s"Unknown operator $s.")

    @tailrec
    def loop(acc: Map[String, Node]): Graph = lines.nextOption() match
      case None => Graph(acc)
      case Some(line) => line match
        case s"$name: $left $op $right" => loop(acc + (name -> Branch(name, getOperator(op), left, right)))
        case s"$name: $value" => loop(acc + (name -> Leaf(name, value.toLong)))
        case _ => loop(acc)

    loop(Map())

  def squeezeGraph(graph: Graph): Long =
    @tailrec
    def loop(currentGraph: Graph): Long =
      val ripeNodes: Set[(Branch, (Leaf, Leaf))] = currentGraph.ripeNodes
      val newLeaves: Map[String, Node] = ripeNodes.map{
        case (Branch(name, op, _, _), (Leaf(_, value1), Leaf(_, value2))) => name -> Leaf(name, op(value1, value2))
      }.toMap
      newLeaves.get(Root) match
        case Some(Leaf(_, value)) => value
        case _ =>
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

end Day21a
