package org.p99.scala

object P28 {
  def group[T](groupList: List[Int], ls: List[T]) = {
    val first = P26.combinations(groupList.head, ls).map(List(_))
    groupList.tail.foldLeft(first) { (x, size) => x.flatMap(y => P26.combinations(size, ls.filterNot(y.flatten.toSet)).map(z => z :: y)) }
  }
}
