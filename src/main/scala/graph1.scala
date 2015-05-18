import scala.collection.SortedSet

abstract class GraphBase[T, U] {
  case class Edge(n1: Node, n2: Node, value: U) {
    def toTuple = (n1.value, n2.value, value)
    override def toString = n1.value + connector + n2.value
    def toStringLabel: String = n1.value + connector + n2.value + "/" + value

  }

  case class Node(value: T) {
    var adj: Set[Edge] = Set.empty
    // neighbors are all nodes adjacent to this node.
    def neighbors: Set[Node] = adj.map(edgeTarget(_, this).get)
  }

  var nodes: Map[T, Node] = Map()
  var edges: Set[Edge] = Set.empty

  // If the edge E connects N to another node, returns the other node,
  // otherwise returns None.
  def edgeTarget(e: Edge, n: Node): Option[Node]

  override def equals(o: Any) = o match {
    case g: GraphBase[_,_] => (nodes.keys.toSet -- g.nodes.keys.toSet) == Set.empty && (edges.map(_.toTuple).toSet -- g.edges.map(_.toTuple).toSet) == Set.empty
    case _ => false
  }
  def addNode(value: T) = {
    if (!(nodes contains value)) {
      val n = new Node(value)
      nodes = Map(value -> n) ++ nodes
      n
    } else
      nodes(value)
  }
  def addEdge(n1: T, n2: T, value: U)

  def toTermForm: (List[T], Set[(T, T, U)]) = (nodes.keys.toList, edges.map(_.toTuple))
  def toAdjacentForm: List[(T, Set[(T, U)])] = nodes.map{ case (value: T, node: Node) => (value, node.adj.map(x => (x.n2.value, x.value))) }.toList
  val connector: String

  def connectedNodes: Set[T] = edges.flatMap(x => Set(x.n1.value, x.n2.value))
  def disconnectedNodes: Set[T] = nodes.keys.toSet -- connectedNodes

  override def toString: String =
    "[" + (disconnectedNodes.map(_.toString) ++ edges.map(_.toString)).mkString(",") + "]"

  def toStringLabel: String =
    "[" + (disconnectedNodes.map(_.toString) ++ edges.map(_.toStringLabel)).mkString(",") + "]"

  def findPaths(start: T, end: T): Set[List[T]] = {
    def findPathsAux(start: T, end: T, visited: Set[T]): Set[List[T]] = {
      if (start == end)
        Set(List(start))
      else {
        nodes(start).neighbors.filterNot(visited contains _.value).flatMap(x => findPathsAux(x.value, end, visited + start)).map(start :: _)
      }
    }
    findPathsAux(start, end, Set.empty)
  }

  def findCycles(start: T): Set[List[T]] = {
    nodes(start).neighbors.flatMap(x => findPaths(x.value, start)).map(start :: _).filter(_.length > 3)
  }


}

class Graph[T, U] extends GraphBase[T, U] {

  def copy(): Graph[T, U] = {
    val g = new Graph[T, U]()
    nodes.keys.foreach(g.addNode)
    edges.foreach(x => g.addEdge(x.n1.value, x.n2.value, x.value))
    g
  }

  override def equals(o: Any) = o match {
    case g: Graph[_,_] => super.equals(g)
    case _ => false
  }
  implicit class EdgeOps(e: Edge) {
    def equals(e2: Edge) = {
      if((e.n1 == e2.n1 && e.n2 == e2.n2) || (e.n1 == e2.n2 && e.n2 == e2.n1)) true else false
    }
  }

  def edgeTarget(e: Edge, n: Node): Option[Node] =
    if (e.n1 == n) Some(e.n2)
    else if (e.n2 == n) Some(e.n1)
    else None

  def addEdge(n1: T, n2: T, value: U) = {
    val e = new Edge(nodes(n1), nodes(n2), value)
    if(!(edges contains new Edge(nodes(n2), nodes(n1), value)))
      edges += e
    nodes(n1).adj += e
    nodes(n2).adj += e
  }

  val connector = "-"

  def spanningTrees: Set[Graph[T,U]] = {
    def treesFrom(g: Graph[T, U]): Set[Graph[T, U]] = {
      def xor(x:Boolean, y:Boolean) = (x && !y) || (y && !x)
      val notInGraphEdges = edges.filter(x => xor(g.nodes.isDefinedAt(x.n1.value), g.nodes.isDefinedAt(x.n2.value)))
      if (notInGraphEdges.isEmpty) {
        Set(g)
      }
      else {
        val newGraphs = notInGraphEdges.map { edge =>
          val newGraph = g.copy()
          newGraph.addNode(edge.n1.value)
          newGraph.addNode(edge.n2.value)
          newGraph.addEdge(edge.n1.value, edge.n2.value, edge.value)
          newGraph
        }
        newGraphs.flatMap(treesFrom)
      }
    }
    val g = new Graph[T, U]
    g.addNode(nodes.values.head.value)
    treesFrom(g)
  }

  def minimalSpanningTree(implicit f: (U) => Ordered[U]): Graph[T, U] = {
    def treeFrom(g: Graph[T, U]): Graph[T, U] = {
      def xor(x:Boolean, y:Boolean) = (x && !y) || (y && !x)
      val notInGraphEdges = edges.filter(x => xor(g.nodes.isDefinedAt(x.n1.value), g.nodes.isDefinedAt(x.n2.value)))
      if (notInGraphEdges.isEmpty) {
        g
      }
      else {
        val newGraph = g.copy()
        val edge: Edge = notInGraphEdges.minBy(_.value)
        newGraph.addNode(edge.n1.value)
        newGraph.addNode(edge.n2.value)
        newGraph.addEdge(edge.n1.value, edge.n2.value, edge.value)
        treeFrom(newGraph)
      }
    }
    val g = new Graph[T, U]
    g.addNode(nodes.values.head.value)
    treeFrom(g)
  }
}

class Digraph[T, U] extends GraphBase[T, U] {
  override def equals(o: Any) = o match {
    case g: Digraph[_, _] => super.equals(g)
    case _ => false
  }

  def edgeTarget(e: Edge, n: Node): Option[Node] =
    if (e.n1 == n) Some(e.n2)
    else None

  def addArc(source: T, dest: T, value: U) = {
    val e = new Edge(nodes(source), nodes(dest), value)
    edges += e
    nodes(source).adj += e
  }

  def addEdge(source: T, dest: T, value: U) = addArc(source, dest, value)

  val connector = ">"
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

  def fromString[G <: GraphBase[Char, Unit]](s: String, edgeSeperator: Char, g: G): G = {
    val pairs = s.substring(1, s.length - 1).split(',').map(_.trim)
    val nodes = pairs.map(_.split(edgeSeperator).map(_(0)))

    def addNode(value: Char): Unit = {
      if(!g.nodes.contains(value))
        g.addNode(value)
    }

    def addEdge(edge: Array[Char]): Unit = {
      if(edge.length == 2) {
        g.addNode(edge(0))
        g.addNode(edge(1))
        g.addEdge(edge(0), edge(1), ())
      }
      else
        g.addNode(edge(0))
    }
    nodes.foreach(addEdge)
    g
  }

  def fromStringLabel[G <: GraphBase[Char, Int]](s: String, edgeSeperator: Char, g: G): G = {
    val pairs = s.substring(1, s.length - 1).split(',').map(_.trim)

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
        addNode(splitByNode(0)(0))
        g.addEdge(splitByNode(0)(0), splitByLabel(0)(0), splitByLabel(1).toInt)
      }
    }
    pairs.foreach(addEdge)
    g
  }
}

object Graph extends GraphObjBase {
  type GraphClass[T, U] = Graph[T, U]

  def termLabel[T, U](nodes: List[T], edges: List[(T,T,U)]) = {
    val g = new Graph[T, U]
    nodes.foreach(g.addNode)
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

  def fromString(s: String): Graph[Char, Unit] = super.fromString[Graph[Char, Unit]](s, '-', new Graph[Char, Unit])
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
  def fromString(s: String): Digraph[Char, Unit] = super.fromString[Digraph[Char, Unit]](s, '>', new Digraph[Char, Unit])
  def fromStringLabel(s: String): Digraph[Char, Int] = super.fromStringLabel[Digraph[Char, Int]](s, '>', new Digraph[Char, Int])


}

object Test extends App {

  println(Graph.fromStringLabel("[b-c/4, f-c/10, g-h/6, d, f-b/9, k-f/10, b-f/9]").toStringLabel)
  println(Digraph.fromStringLabel("[b>c/4, f>c/10, g>h/6, d, f>b/9, k>f/10, h>g/5]").toStringLabel)
  println(Digraph.fromStringLabel("[p>q/9, m>q/7, k, p>m/5]").findPaths('q', 'p'))
  println(Graph.fromStringLabel("[p-q/9, m-q/7, k, p-m/5]").findPaths('p', 'q'))
  println(Graph.fromString("[b-c, f-c, g-h, d, f-b, k-f, h-g]").findCycles('f'))
  println(Graph.fromString("[a-b, b-c, a-c]").spanningTrees)
  val g = Graph.term(List('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h'),
    List(('a', 'b'), ('a', 'd'), ('b', 'c'), ('b', 'e'),
      ('c', 'e'), ('d', 'e'), ('d', 'f'), ('d', 'g'),
      ('e', 'h'), ('f', 'g'), ('g', 'h')))
  println(g.spanningTrees.size)
  println(g.minimalSpanningTree)
  println(Graph.fromStringLabel("[a-b/1, b-c/2, a-c/3]").minimalSpanningTree.toStringLabel)
}