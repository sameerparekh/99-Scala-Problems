package org.p99.scala

import org.scalatest._

class P16Spec extends UnitSpec {
  "drop" should "drop every Nth element of a list" in {
    P16.drop(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) should be
    List('a, 'b, 'd, 'e, 'g, 'h, 'j, 'k)
  }
}

