package org.p99.scala

import org.scalatest._

class P07Spec extends UnitSpec {

  "flatten" should "flatten a list" in {
    P07.flatten(List('a, List('b, 'c, List('x)), List('d'))) should be (List('a, 'b, 'c, 'x, 'd))
  }

}

