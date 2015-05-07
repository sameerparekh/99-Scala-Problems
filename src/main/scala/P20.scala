package org.p99.scala

object P20 {
  def removeAt[T](n: Int, ls: List[T]) = {
    def removeAtAux[T](n: Int, left: List[T], right: List[T]): (List[T], T) = (n, right) match {
      case (_, Nil) => throw new NoSuchElementException
      case (0, _) => (left ::: right.tail, right.head)
      case (x, _) => removeAtAux(x - 1, left :+ right.head, right.tail)
    }
    removeAtAux(n, List(), ls)
  }
}
