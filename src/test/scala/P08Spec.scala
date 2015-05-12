package org.p99.scala

import org.scalatest._

class P08Spec extends UnitSpec {

  "compress" should "eliminate consecutive duplicates" in {
    P08.compress(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) should be (List('a, 'b, 'c, 'a, 'd, 'e))
  }

}

