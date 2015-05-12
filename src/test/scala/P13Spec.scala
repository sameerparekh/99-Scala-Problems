package org.p99.scala

import org.scalatest._

class P13Spec extends UnitSpec {
  "encodeDirect" should "rle a list" in {
    P13.encodeDirect(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) should be
    List((4,'a), 'b, (2,'c), (2,'a), 'd, (4,'e))
  }
}

