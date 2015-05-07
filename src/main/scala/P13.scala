package org.p99.scala

object P13 {
  def encodeDirect[T](l: List[T]) = {
    def encodeAux[T](l: List[T], cur: Option[(Int, T)]): List[(Int, T)] = (l, cur) match {
      case (Nil, Some(x)) => List(x)
      case (Nil, None) => List()
      case (x :: xs, None) => encodeAux(xs, Some((1, x)))
      case (x :: xs, Some((c: Int, y))) => if (x == y) encodeAux(xs, Some((c + 1, y))) else (c, y) :: encodeAux(l, None)
    }
    encodeAux(l, None)
  }


}
