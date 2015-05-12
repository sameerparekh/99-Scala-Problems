package org.p99.scala

object P24 {
  def lotto(n: Int, range: Int) = P23.randomSelect(n, P22.range(1, range + 1))
}
