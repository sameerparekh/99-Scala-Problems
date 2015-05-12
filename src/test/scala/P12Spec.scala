package org.p99.scala

import org.scalatest._

class P12Spec extends UnitSpec {
  "decode" should "decode an rle list" in {
    P12.decode(List((4, 'a), (1, 'b), (2, 'c), (2, 'a), (1, 'd), (4, 'e))) should be
    List((4,'a), (1,'b), (2,'c), (2,'a), (1,'d), (4,'e))
  }
}

