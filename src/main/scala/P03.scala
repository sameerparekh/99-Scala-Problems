package org.p99.scala

object P03 {
  def nth[T](n: Int, l: List[T]): T =
    (n, l) match {
      case (x, _) if x < 0 => throw new NoSuchElementException
      case (0, h :: _)  => h
      case (x, _ :: tail) => nth(x - 1, tail)
      case (_, Nil) => throw new NoSuchElementException
    }
}
