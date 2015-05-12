package org.p99.scala

import org.scalatest._

class P21Spec extends UnitSpec {

  "insert" should "insert an element at a given position into a list" in {
    P21.insertAt('new, 1, List('a, 'b, 'c, 'd)) should be
    List('a, 'new, 'b, 'c, 'd)
  }

}

