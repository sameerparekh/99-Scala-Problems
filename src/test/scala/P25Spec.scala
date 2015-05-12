package org.p99.scala

import org.scalatest._

class P25Spec extends UnitSpec {

  "randomPermute" should "generate a random permutation of the elements of a list" in {
    val permuted = P25.randomPermute(List('a, 'b, 'c, 'd, 'e, 'f))
    permuted should have length 6
    permuted should contain allOf ('a, 'b, 'c, 'd, 'e, 'f)
  }

}

