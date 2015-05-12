package org.p99.scala

import org.scalatest._

class P11Spec extends UnitSpec {
  "encodeModified" should "modified rle a list" in {
    P11.encodeModified(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) should be
      List((4,'a), 'b, (2,'c), (2,'a), 'd, (4,'e))
  }
}

