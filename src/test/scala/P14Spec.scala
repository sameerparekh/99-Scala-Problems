package org.p99.scala

import org.scalatest._

class P14Spec extends UnitSpec {
  "duplicate" should "duplicate elements of a list" in {
    P14.duplicate(List('a, 'b, 'c, 'c, 'd)) should be
    List('a, 'a, 'b, 'b, 'c, 'c, 'c, 'c, 'd, 'd)
  }
}

