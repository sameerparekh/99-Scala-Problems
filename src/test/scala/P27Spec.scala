package org.p99.scala

import org.scalatest._

class P27Spec extends UnitSpec {

  "group3" should "group elements into disjoint subsets" in {
    val people = List("Aldo", "Beat", "Carla", "David", "Evi", "Flip", "Gary", "Hugo", "Ida")
    val grouped = P27.group3(people)
    all (grouped) should have length 3
    grouped.foreach(group => group.flatten should contain theSameElementsAs people)
  }

}

