package org.p99.scala

object P19 {
  def rotate[T](n: Int, l: List[T]): List[T] = {
    val nBounded = if (l.isEmpty) 0 else n % l.length
    if (nBounded < 0) rotate(nBounded + l.length, l)
    else l.drop(nBounded) ::: l.take(nBounded)
  }
}
