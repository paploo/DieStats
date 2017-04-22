package net.paploo.diestats.statistics.util

object Ordering {

  class SeqOrdering[A](implicit ord: Ordering[A]) extends Ordering[Seq[A]] {

    override def compare(x: Seq[A], y: Seq[A]): Int = {
      val as = x.sorted.iterator
      val bs = y.sorted.iterator

      // Find the first non-matching element.
      while(as.hasNext && bs.hasNext) {
        val cmp = ord.compare(as.next(), bs.next())
        if (cmp != 0) return cmp
      }

      // If we got here, they were the same up until one ran out of elements, so it just comes down to size.
      as.length - bs.length
    }

  }

  def seqOrdering[A](implicit ord: Ordering[A]): Ordering[Seq[A]] = new SeqOrdering[A]()

  trait Implicits {

    implicit def seqOrdering[A](implicit ord: Ordering[A]): Ordering[Seq[A]] = Ordering.seqOrdering

  }
  object Implicits extends Implicits

}
