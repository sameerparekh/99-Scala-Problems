package org.p99.scala

import org.scalatest._

class P23Spec extends UnitSpec {

  "randomSelect" should "extract a given number of random elements from a list" in {
    P23.randomSelect(3, List('a, 'b, 'c, 'd, 'f, 'g, 'h)) should have size 3
  }

}

