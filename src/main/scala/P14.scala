package org.p99.scala

object P14 {
  def duplicate[T](l: List[T]): List[T] = l.flatMap(x => List(x, x))
}
