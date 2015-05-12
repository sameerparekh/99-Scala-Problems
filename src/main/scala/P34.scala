package org.p99.scala

object P34 {
  def totient(m: Int): Int = {
    (1 to m).count(P33.isCoprimeTo(m, _))
  }
}
