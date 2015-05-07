package org.p99.scala

object P07 {
  def flatten(l: List[Any]): List[Any] = if (l.nonEmpty)
    l.head match {
      case ls: List[_] => flatten(ls) ::: flatten(l.tail)
      case _ => l.head :: flatten(l.tail)
    }
  else
    l
}
