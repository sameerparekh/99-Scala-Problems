package org.p99.scala

import org.scalatest._

class P17Spec extends UnitSpec {

  "split" should "split a list into two parts" in {
    P17.split(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) should be
    (List('a, 'b, 'c),List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
  }

}

