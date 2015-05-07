package org.p99.scala

object P04 {
  def length[T](l: List[T]): Int = l match {
    case Nil => 0
    case _ :: tail => 1 + length(tail)
  }

  def lengthTail[T](l: List[T]): Int = {
    def lengthAux(l: List[T], c: Int): Int = l match {
      case Nil => c
      case _ :: tail => lengthAux(tail, c + 1)
    }
    lengthAux(l, 0)
  }
}
