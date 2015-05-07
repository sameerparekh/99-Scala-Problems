package org.p99.scala

object P18 {
  def slice[T](i: Int, k: Int, l: List[T]) = l.drop(i).take(k - i)
}
