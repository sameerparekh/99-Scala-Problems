package org.p99.scala

object P27 {

  def group3[T](ls: List[T]) = {
    P26.combinations(2, ls).map(List(_)).flatMap(x => P26.combinations(3, ls.filterNot(x.flatten.toSet)).map(y => y :: x)).flatMap(x => P26.combinations(4, ls.filterNot(x.flatten.toSet)).map(y => y :: x))
  }


}
