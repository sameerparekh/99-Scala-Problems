package org.p99.scala

import org.scalatest._

class P20Spec extends UnitSpec {
  "removeAt" should "remove the kth element from a list" in {
    P20.removeAt(1, List('a, 'b, 'c, 'd)) should be
    (List('a, 'c, 'd),'b)
  }

}

