package net.paploo.diestats.statistics.util

object Ordering {

  class SeqOrdering[A](implicit ord: Ordering[A]) extends Ordering[Seq[A]] {

    override def compare(x: Seq[A], y: Seq[A]): Int = {
      val as = x.iterator
      val bs = y.iterator

      // Find the first non-matching element.
      while(as.hasNext && bs.hasNext) {
        val cmp = ord.compare(as.next(), bs.next())
        if (cmp != 0) return cmp
      }

      // If we got here, they were the same up until one ran out of elements, so it just comes down to size.
      x.length - y.length
    }

  }
  implicit def SeqOrdering[A](implicit ord: Ordering[A]): Ordering[Seq[A]] = new SeqOrdering[A]()

}
