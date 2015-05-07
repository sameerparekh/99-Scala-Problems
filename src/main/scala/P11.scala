package org.p99.scala

object P11 {
  def encodeModified[T](l: List[T]) = P09.pack(l).map(x => x.length match {
    case 1 => x.head
    case l => (l, x.head)
  } )
}
