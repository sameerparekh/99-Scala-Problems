package org.p99.scala

object P16 {
  def drop[T](n: Int, l: List[T]): List[T] = l match {
    case Nil => Nil
    case _ => l.take(n - 1) ::: drop(n, l.drop(n))
  }

}
