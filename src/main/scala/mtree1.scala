
class MTree[+T](value: T, children: List[MTree[T]]) {
  override def toString = value.toString + children.map(_.toString).mkString("") + "^"
  def nodeCount: Int = 1 + children.map(_.nodeCount).sum
  def internalPathLength: Int = children.size + children.map(_.internalPathLength * 2).sum
  def postorder: List[T] = children.flatMap(_.postorder) :+ value
  def lispyTree: String = if (children.isEmpty) value.toString else "(" + value.toString + " " + children.map(_.lispyTree).mkString(" ") + ")"
}

object MTree {
  def apply[T](value: T) = new MTree(value, List())

  def apply[T](value: T, children: List[MTree[T]]) = new MTree(value, children)

  def fromString(s: String): MTree[Char] = {
    def fromStringAux(s: String): (MTree[Char], String) = {
      def getChildren(s: String): (List[MTree[Char]], String) = s.toList match {
        case '^' :: t => (Nil, s.tail)
        case _ =>
          val (firstTree, remaining) = fromStringAux(s)
          val (nextChildren, remaining2) = getChildren(remaining)
          (firstTree :: nextChildren, remaining2)
      }
      val value = s.head
      val (children, remaining) = getChildren(s.tail)
      (MTree(value, children), remaining)
    }
    fromStringAux(s)._1
  }

  implicit def stringToMTree(s: String): MTree[Char] = fromString(s)
}

object TestMTree extends App {
  println(MTree('a', List(MTree('f'))).toString)
  println(MTree('a', List(MTree('f', List(MTree('g'))), MTree('c'), MTree('b', List(MTree('d'), MTree('e'))))).toString)
  val y: MTree[Char] = "afg^c^bd^e^f^^^^"
  println(y.lispyTree)
}