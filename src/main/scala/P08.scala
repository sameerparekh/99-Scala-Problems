package org.p99.scala

object P08 {
  def compress[T](l: List[T]): List[T] = l match {
    case x :: y :: xs => if (x == y) compress(y :: xs) else x :: compress(y :: xs)
    case y :: Nil => List(y)
    case Nil => Nil
  }
}
