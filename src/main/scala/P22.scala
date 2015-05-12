package org.p99.scala

object P22 {
  def range(start: Int, end: Int): List[Int] = if (start >= end) List() else start :: range(start + 1, end)
}
