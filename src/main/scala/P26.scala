package org.p99.scala

object P26 extends App {
  def combinations[T](n: Int, ls: List[T]): List[List[T]] = (n, ls) match {
    case (0, _) => List()
    case (1, ls) => ls.map(List(_))
    case (_, x :: xs) => combinations(n - 1, xs).map(y => x :: y) ::: combinations(n, xs)
    case (_, Nil) => List()
  }

  println(combinations(7, List('a, 'b, 'c, 'd, 'e, 'f)))
}
