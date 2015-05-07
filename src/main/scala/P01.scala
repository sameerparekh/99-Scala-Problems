package org.p99.scala

object P01 {
  def last[T](l: List[T]): T = l match {
    case h :: Nil => h
    case _ :: tail => last(tail)
    case _ => throw new NoSuchElementException
  }
}
