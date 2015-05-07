package org.p99.scala

object P15 {
  def duplicateN[T](n: Int, l: List[T]): List[T] = l.flatMap(x => List.fill(n)(x))
}
