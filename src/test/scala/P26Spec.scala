package org.p99.scala

import org.scalatest._

class P26Spec extends UnitSpec {

  "combinations" should "generate combinations from a list" in {
    P26.combinations(3, List('a, 'b, 'c, 'd, 'e, 'f)) should have length 20 // 6 choose 3
    P26.combinations(5, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h)) should have length 56 // 8 choose 5
    P26.combinations(8, List('a, 'b)) should have length 0 // 0 ways to pick greater than the list length
  }
}

