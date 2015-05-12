package org.p99.scala

import org.scalatest._

class P04Spec extends UnitSpec {

  "length" should "Return the length of the list" in {
    assert(P04.length(List('a, 'b)) === 2)
  }

  "lengthTail" should "Return the length of the list" in {
    assert(P04.lengthTail(List('a, 'b)) === 2)
  }
}

