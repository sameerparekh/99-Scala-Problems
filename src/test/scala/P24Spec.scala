package org.p99.scala

import org.scalatest._

class P24Spec extends UnitSpec {
  "lotto" should "draw N different random numbers in the set 1..M" in {
    val lot = P24.lotto(6, 49)
    lot should have length 6
    all (lot) should be <= 49
    all (lot) should be >= 1
  }

}

