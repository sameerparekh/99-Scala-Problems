package org.p99.scala

object P02 {
  def penultimate[A](l: List[A]): A = l match {
    case pen :: _ :: Nil => pen
    case _ :: tail => penultimate(tail)
    case _ => throw new NoSuchElementException
  }
}
