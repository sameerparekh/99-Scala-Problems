package org.p99.scala

import org.scalatest._

class P22Spec extends UnitSpec {
  "range" should "create a list containing integers in a range" in {
    P22.range(4, 9) should be
    List(4, 5, 6, 7, 8, 9)
  }

}

