

abstract class GraphBase[T, U] {
  case class Edge(n1: Node, n2: Node, value: U) {
    def toTuple = (n1.value, n2.value, value)
  }
  case class Node(value: T) {
    var adj: List[Edge] = Nil
    // neighbors are all nodes adjacent to this node.
    def neighbors: List[Node] = adj.map(edgeTarget(_, this).get)
  }

  var nodes: Map[T, Node] = Map()
  var edges: List[Edge] = Nil

  // If the edge E connects N to another node, returns the other node,
  // otherwise returns None.
  def edgeTarget(e: Edge, n: Node): Option[Node]

  override def equals(o: Any) = o match {
    case g: GraphBase[_,_] => (nodes.keys.toSet -- g.nodes.keys.toSet) == Set.empty && (edges.map(_.toTuple).toSet -- g.edges.map(_.toTuple).toSet) == Set.empty
    case _ => false
  }
  def addNode(value: T) = {
    val n = new Node(value)
    nodes = Map(value -> n) ++ nodes
    n
  }
  def addEdge(n1: T, n2: T, value: U)

  def toTermForm: (List[T], List[(T, T, U)]) = (nodes.keys.toList, edges.map(_.toTuple))
  def toAdjacentForm: List[(T, List[T])] = nodes.map{ case (value: T, node: Node) => (value, node.neighbors.map(_.value)) }.toList
}

class Graph[T, U] extends GraphBase[T, U] {
  override def equals(o: Any) = o match {
    case g: Graph[_,_] => super.equals(g)
    case _ => false
  }

  def edgeTarget(e: Edge, n: Node): Option[Node] =
    if (e.n1 == n) Some(e.n2)
    else if (e.n2 == n) Some(e.n1)
    else None

  def addEdge(n1: T, n2: T, value: U) = {
    val e = new Edge(nodes(n1), nodes(n2), value)
    edges = e :: edges
    nodes(n1).adj = e :: nodes(n1).adj
    nodes(n2).adj = e :: nodes(n2).adj
  }
}

class Digraph[T, U] extends GraphBase[T, U] {
  override def equals(o: Any) = o match {
    case g: Digraph[_,_] => super.equals(g)
    case _ => false
  }

  def edgeTarget(e: Edge, n: Node): Option[Node] =
    if (e.n1 == n) Some(e.n2)
    else None

  def addArc(source: T, dest: T, value: U) = {
    val e = new Edge(nodes(source), nodes(dest), value)
    edges = e :: edges
    nodes(source).adj = e :: nodes(source).adj
  }

  def addEdge(source: T, dest: T, value: U) = addArc(source, dest, value)
}

abstract class GraphObjBase {
  type GraphClass[T, U]
  def addLabel[T](edges: List[(T, T)]) =
    edges.map(v => (v._1, v._2, ()))
  def term[T](nodes: List[T], edges: List[(T,T)]) =
    termLabel(nodes, addLabel(edges))
  def termLabel[T, U](nodes: List[T], edges: List[(T,T,U)]): GraphClass[T, U]
  def addAdjacentLabel[T](nodes: List[(T, List[T])]) =
    nodes.map(a => (a._1, a._2.map((_, ()))))
  def adjacent[T](nodes: List[(T, List[T])]) =
    adjacentLabel(addAdjacentLabel(nodes))
  def adjacentLabel[T, U](nodes: List[(T, List[(T,U)])]): GraphClass[T, U]

  def fromString[G <: GraphBase[Char, Null]](s: String, edgeSeperator: Char, g: G): G = {
    val pairs = s.substring(1, s.length - 2).split(',').map(_.trim)
    val nodes = pairs.map(_.split(edgeSeperator).map(_(0)))

    def addNode(value: Char): Unit = {
      if(!g.nodes.contains(value))
        g.addNode(value)
    }

    def addEdge(edge: Array[Char]): Unit = {
      if(edge.length == 2) {
        g.addNode(edge(0))
        g.addNode(edge(1))
        g.addEdge(edge(0), edge(1), null)
      }
      else
        g.addNode(edge(0))
    }
    nodes.foreach(addEdge)
    g
  }

  def fromStringLabel[G <: GraphBase[Char, Int]](s: String, edgeSeperator: Char, g: G): G = {
    val pairs = s.substring(1, s.length - 2).split(',').map(_.trim)

    def addNode(value: Char): Unit = {
      if(!g.nodes.contains(value))
        g.addNode(value)
    }

    def addEdge(edge: String) = {
      val splitByNode = edge.split(edgeSeperator)
      if(splitByNode.length == 1) {
        addNode(splitByNode(0)(0))
      }
      else {
        val splitByLabel = splitByNode(1).split('/')
        addNode(splitByLabel(0)(0))
        g.addEdge(splitByNode(0)(0), splitByLabel(0)(0), splitByLabel(1).toInt)
      }
    }
    g
  }
}

object Graph extends GraphObjBase {
  type GraphClass[T, U] = Graph[T, U]

  def termLabel[T, U](nodes: List[T], edges: List[(T,T,U)]) = {
    val g = new Graph[T, U]
    nodes.map(g.addNode)
    edges.foreach(v => g.addEdge(v._1, v._2, v._3))
    g
  }
  def adjacentLabel[T, U](nodes: List[(T, List[(T,U)])]) = {
    val g = new Graph[T, U]
    for ((v, a) <- nodes) g.addNode(v)
    for ((n1, a) <- nodes; (n2, l) <- a) {
      if (!g.nodes(n1).neighbors.contains(g.nodes(n2)))
        g.addEdge(n1, n2, l)
    }
    g
  }

  def fromString(s: String): Graph[Char, Null] = super.fromString[Graph[Char, Null]](s, '-', new Graph[Char, Null])
  def fromStringLabel(s: String): Graph[Char, Int] = super.fromStringLabel[Graph[Char, Int]](s, '-', new Graph[Char, Int])

}

object Digraph extends GraphObjBase {
  type GraphClass[T, U] = Digraph[T, U]

  def termLabel[T, U](nodes: List[T], edges: List[(T,T,U)]) = {
    val g = new Digraph[T, U]
    nodes.map(g.addNode)
    edges.foreach(v => g.addArc(v._1, v._2, v._3))
    g
  }
  def adjacentLabel[T, U](nodes: List[(T, List[(T,U)])]) = {
    val g = new Digraph[T, U]
    for ((n, a) <- nodes) g.addNode(n)
    for ((s, a) <- nodes; (d, l) <- a) g.addArc(s, d, l)
    g
  }
  def fromString(s: String): Digraph[Char, Null] = super.fromString[Digraph[Char, Null]](s, '>', new Digraph[Char, Null])
  def fromStringLabel(s: String): Digraph[Char, Int] = super.fromStringLabel[Digraph[Char, Int]](s, '>', new Digraph[Char, Int])


}

object Test extends App {
  println(Graph.fromString("[b-c, f-c, g-h, d, f-b, k-f, h-g]").toAdjacentForm)
}