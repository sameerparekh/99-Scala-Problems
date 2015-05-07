package org.p99.scala

object P12 {
  def decode[T](l: List[(Int, T)]): List[T] = l match {
    case Nil => Nil
    case (0, x) :: xs => decode(xs)
    case (c: Int, x) :: xs => x :: decode((c - 1, x) :: xs)
  }
}
