package org.p99.scala

object P09 {
  def pack[T](l: List[T]): List[List[T]] = {
    def packAux[T](l: List[T], cur: List[T]): List[List[T]] = (l, cur) match {
      case (Nil, x) => List(x)
      case (x :: xs, Nil) => packAux(xs, List(x))
      case (x :: xs, c :: _) => if (x == c) packAux(xs, x :: cur) else cur :: packAux(l, Nil)
    }
    packAux(l, List())
  }
}
