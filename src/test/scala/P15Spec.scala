package org.p99.scala

import org.scalatest._

class P15Spec extends UnitSpec {
  "duplicateN" should "duplicate the elements of a list a given number of times" in {
    P15.duplicateN(3, List('a, 'b, 'c, 'c, 'd)) should be
    List('a, 'a, 'a, 'b, 'b, 'b, 'c, 'c, 'c, 'c, 'c, 'c, 'd, 'd, 'd)
  }

}

