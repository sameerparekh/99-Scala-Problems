package org.p99.scala

object P10 {
  def encode[T](l: List[T]) = P09.pack(l).map(x => (x.length, x.head))
}
