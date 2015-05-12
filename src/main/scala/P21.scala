package org.p99.scala

object P21 {
  def insertAt[T](x: T, n: Int, ls: List[T]): List[T] = (n, ls) match {
    case (0, l) => x :: l
    case (p, y :: ys) => y :: insertAt(x, p - 1, ys)
    case (p, Nil) => throw new NoSuchElementException
  }
}
