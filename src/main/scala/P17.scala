package org.p99.scala

object P17 {
  def split[T](n: Int, l: List[T]): (List[T], List[T]) = (l.take(n), l.drop(n))

}
