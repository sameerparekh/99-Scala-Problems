package org.p99.scala

object P05 {
  def reverse[T](l: List[T]): List[T] = {
    def reverseAux(start: List[T], reversed: List[T]): List[T] = start match {
      case Nil => reversed
      case h :: tail => reverseAux(tail, h :: reversed)
    }
    reverseAux(l, List())
  }
}
