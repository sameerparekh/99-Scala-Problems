package org.p99.scala

import org.scalatest._

class P28Spec extends UnitSpec {

  "group" should "group elements into disjoint subsets" in {
    val people = List("Aldo", "Beat", "Carla", "David", "Evi", "Flip", "Gary", "Hugo", "Ida")
    val grouped = P28.group(List(2, 2, 5), people)
    all (grouped) should have length 3
    grouped.foreach(group => group.flatten should contain theSameElementsAs people)
  }


}

