
sealed abstract class Tree[+T] {
  def isSymmetric: Boolean

  def addValue[U >: T](x: U)(implicit ev1: U => Ordered[U]): Tree[U]

  def size: Int

  def isBalanced: Boolean

  def isHeightBalanced: Boolean

  def height: Int

  def leafCount: Int

  def leafList: List[T]

  def internalList: List[T]

  def atLevel(level: Int): List[T]

  def layoutBinaryTree: Tree[T]
  def layoutBinaryTree2: Tree[T]
  def preorder: List[T]
  def inorder: List[T]
  def toDotString: String

}



abstract class GenericNode[+T](value: T, left: Tree[T], right: Tree[T]) extends Tree[T] {
  override def toString = if (left == End && right == End) s"$value" else s"$value($left,$right)"
  def toDotString = value.toString + left.toDotString + right.toDotString

  def isSymmetric = Tree.isMirrorOf(left, right)

  def addValue[U >: T](x: U)(implicit ev1: U => Ordered[U]) = if (x < value) Node(value, left.addValue(x), right) else Node(value, left, right.addValue(x))

  def size = 1 + left.size + right.size

  def isBalanced: Boolean = Math.abs(left.size - right.size) <= 1 && left.isBalanced && right.isBalanced

  def isHeightBalanced = Math.abs(left.height - right.height) <= 1 && left.isHeightBalanced && right.isHeightBalanced

  def height = 1 + Math.max(left.height, right.height)

  def isLeaf = left == End && right == End

  def leafCount = if (this.isLeaf) 1 else left.leafCount + right.leafCount

  def leafList = if (this.isLeaf) List(value) else left.leafList ::: right.leafList

  def internalList = if (this.isLeaf) left.internalList ::: right.internalList else value :: left.internalList ::: right.internalList

  def atLevel(level: Int) = if (level == 0) List(value) else left.atLevel(level - 1) ::: right.atLevel(level - 1)

  def layoutBinaryTree: Tree[T] = {
    def layoutBinaryTreeAux(n: Tree[T], height: Int, position: Int): Tree[T] = n match {
      case End => End
      case Node(contents, l, r) =>
        val leftTree = layoutBinaryTreeAux(l, height + 1, position)
        val thisPosition = leftTree.size + position + 1
        val rightTree = layoutBinaryTreeAux(r, height + 1, thisPosition)
        PositionedNode(contents, leftTree, rightTree, height, thisPosition)
    }
    layoutBinaryTreeAux(this, 1, 0)
  }

  implicit class MathOps(a: Int) {
    def pow(b: Int): Int = {
      require(b >= 0, "exponent must be non-negative integer")
      b match {
        case 0 => 1
        case _ => a * (a pow (b - 1))
      }
    }
  }

  def layoutBinaryTree2: Tree[T] = {
    val height = this.height
    def layoutBinaryTreeAux(n: Tree[T], level: Int, position: Int): Tree[T] = n match {
      case End => End
      case Node(contents, l, r) =>
        val distance = 2 pow (height - level)
        l match {
          case End =>
            val myPosition = if (position < 1) 1 else position
            val rightTree = if (r != End) layoutBinaryTreeAux(r, level + 1, myPosition + distance / 2) else End
            PositionedNode(contents, End, rightTree, level, myPosition)
          case _ =>
            val leftPosition = if (position - distance < 1) 1 else position - distance / 2
            val leftTree = layoutBinaryTreeAux(l, level + 1, leftPosition)
            val realLeftPosition = leftTree match { case PositionedNode(_, _, _, x, y) => y }
            val rightTree = layoutBinaryTreeAux(r, level + 1, realLeftPosition + distance)
            val myPosition = realLeftPosition + distance / 2
            PositionedNode(contents, leftTree, rightTree, level, myPosition)
        }
    }
    layoutBinaryTreeAux(this, 1, 1)
  }

  def preorder = value :: left.preorder ::: right.preorder
  def inorder = (left.inorder :+ value) ::: right.inorder
}

case class Node[+T](value: T, left: Tree[T], right: Tree[T]) extends GenericNode[T](value, left, right)

case class PositionedNode[+T](value: T, left: Tree[T], right: Tree[T], x: Int, y: Int) extends GenericNode[T](value, left, right) {
  override def toString = "T[" + x.toString + "," + y.toString + "](" + value.toString + " " + left.toString + " " + right.toString + ")"
}

case object End extends Tree[Nothing] {
  override def toString = ""
  def toDotString = "."

  val isSymmetric = true

  def addValue[U](x: U)(implicit ev1: U => Ordered[U]) = Node(x)

  val size = 0
  val isBalanced = true
  val isHeightBalanced = true
  val height = 0
  val leafCount = 0
  val leafList = Nil
  val internalList = Nil
  def atLevel(level: Int) = Nil
  val layoutBinaryTree = End
  val layoutBinaryTree2 = End
  val inorder = Nil
  val preorder = Nil

}

object Node {
  def apply[T](value: T): Node[T] = Node(value, End, End)

}

object Tree {
  def cBalanced[T](size: Int, contents: T): List[Tree[T]] = size match {
    case 0 => List(End)
    case _ =>
      val sizeLeft = (size - 1) / 2
      val sizeRight = size - 1 - sizeLeft
      assert(Math.abs(sizeLeft - sizeRight) <= 1)
      val list = if (sizeLeft == sizeRight) {
        val subTrees = cBalanced(sizeLeft, contents)
        for {
          leftTree <- subTrees
          rightTree <- subTrees
        } yield List(Node(contents, leftTree, rightTree))
      } else {
        for {
          leftTree <- cBalanced(sizeLeft, contents)
          rightTree <- cBalanced(sizeRight, contents)
        } yield List(Node(contents, leftTree, rightTree), Node(contents, rightTree, leftTree))
      }
      list.flatten
  }

  def hbalTrees[T](height: Int, contents: T): List[Tree[T]] = height match {
    case 0 => List(End)
    case 1 => List(Node(contents, End, End))
    case n if n > 1 =>
      val subTrees = hbalTrees(n - 1, contents) ::: hbalTrees(n - 2, contents)
      for {
        left <- subTrees
        right <- subTrees
        if Math.abs(left.height - right.height) <= 1
        if Math.max(left.height, right.height) == n - 1
      } yield Node(contents, left, right)
  }

  def minHbalNodes(height: Int): Int = height match {
    case 0 => 0
    case 1 => 1
    case n if n > 1 => 1 + minHbalNodes(n - 1) + minHbalNodes(n - 2)
  }

  implicit class MathOps(a: Int) {
    def pow(b: Int): Int = {
      require(b >= 0, "exponent must be non-negative integer")
      b match {
        case 0 => 1
        case _ => a * (a pow (b - 1))
      }
    }
  }

  def maxHbalNodes(height: Int): Int = (2 pow height) - 1

  def maxHbalHeight(nodes: Int): Int = nodes match {
    case 0 => 0
    case 1 => 1
    case n if n > 1 =>
      val heights = for {
        leftNodes <- 1 until nodes
        rightNodes = nodes - leftNodes - 1
        leftHeight = maxHbalHeight(leftNodes)
        rightHeight = maxHbalHeight(rightNodes)
        if Math.abs(leftHeight - rightHeight) <= 1
      } yield 1 + Math.max(leftHeight, rightHeight)
      heights.max
  }

  def hbalTreesWithNodes[T](nodes: Int, contents: T) = {
    val maxHeight = maxHbalHeight(nodes)
    val posHeights = (1 to maxHeight).filter(x => (minHbalNodes(x) to maxHbalNodes(x)) contains nodes)
    posHeights.flatMap(x => hbalTrees(x, contents)).filter(_.size == nodes)
  }

  def isMirrorOf[T](l: Tree[T], r: Tree[T]): Boolean = (l, r) match {
    case (End, End) => true
    case (Node(_, l1, r1), Node(_, l2, r2)) => isMirrorOf(l1, r2) && isMirrorOf(r1, l2)
    case _ => false
  }

  def fromList[T](l: List[T])(implicit ev1: T => Ordered[T]): Tree[T] = l.foldLeft[Tree[T]](End)((tree, value) => tree.addValue(value))

  def symmetricBalancedTrees[T](size: Int, contents: T) = cBalanced(size, contents).filter(_.isSymmetric)

  def completeBinaryTree[T](nodes: Int, contents: T) = {
    def completeBinarySubTree(address: Int): Tree[T] =
      if (address <= nodes)
        Node(contents, completeBinarySubTree(address * 2), completeBinarySubTree(address * 2 + 1))
      else
        End
    completeBinarySubTree(1)
  }

  def string2Tree(s: String): Tree[Char] = {
    def extractTreeString(s: String, start: Int, end: Char): (String,Int) = {
      def updateNesting(nesting: Int, pos: Int): Int = s(pos) match {
        case '(' => nesting + 1
        case ')' => nesting - 1
        case _   => nesting
      }
      def findStringEnd(pos: Int, nesting: Int): Int =
        if (s(pos) == end && nesting == 0) pos
        else findStringEnd(pos + 1, updateNesting(nesting, pos))
      val strEnd = findStringEnd(start, 0)
      (s.substring(start, strEnd), strEnd)
    }
    s.length match {
      case 0 => End
      case 1 => Node(s(0))
      case _ => {
        val (leftStr, commaPos) = extractTreeString(s, 2, ',')
        val (rightStr, _) = extractTreeString(s, commaPos + 1, ')')
        Node(s(0), string2Tree(leftStr), string2Tree(rightStr))
      }
    }
  }

  def preInTree[T](preorder: List[T], inorder: List[T]): Tree[T] = {
    def partition(l: List[T], x: T) = {
      val leftList = l.takeWhile(y => y != x)
      val rightList = l.drop(leftList.size + 1)
      (leftList, rightList)
    }
    preorder match {
      case Nil => End
      case x :: xs =>
        val (leftInorder, rightInorder) = partition(inorder, x)
        val leftPreorder = xs.take(leftInorder.size)
        val rightPreorder = xs.drop(leftInorder.size)
        Node(x, preInTree(leftPreorder, leftInorder), preInTree(rightPreorder, rightInorder))
    }
  }

  def fromDotString(s: String): Tree[Char] = {
    def fromDotStringAux(s: String): (Tree[Char], String) = {
      val value = s.head
      value match {
        case '.' => (End, s.tail)
        case _ =>
          val (leftTree, stringLeft) = fromDotStringAux(s.tail)
          val (rightTree, stringLeftAgain) = fromDotStringAux(stringLeft)
          (Node(value, leftTree, rightTree), stringLeftAgain)
      }
    }
    fromDotStringAux(s)._1
  }
}


object Test extends App {

  println(Tree.fromDotString(Tree.preInTree(List('a', 'b', 'a'), List('b', 'a', 'a')).toDotString))
}