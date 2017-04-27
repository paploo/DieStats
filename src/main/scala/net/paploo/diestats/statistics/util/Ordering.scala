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
  def SeqOrdering[A](implicit ord: Ordering[A]): Ordering[Seq[A]] = new SeqOrdering[A]()

  /**
    * For many domains, a Set that allows repeated elements is ideal; this Ordering
    * treats a Seq[A] like a Set[A] that can have repeated elements, but giving a
    * predictable ordering.
    *
    * For example, coin flips may be represented as Seq('H','T','T'), which, for
    * aggregation/convolution on the count of heads and count of tails, should be
    * treated the same as Seq('T','H','T').
    * @param ord
    * @tparam A
    */
  class SetSeqOrdering[A](implicit ord: Ordering[A]) extends SeqOrdering[A] {
    override def compare(x: Seq[A], y: Seq[A]): Int = super.compare(x.sorted(ord), y.sorted(ord))
  }
  def SetSeqOrdering[A](implicit ord: Ordering[A]): Ordering[Seq[A]] = new SetSeqOrdering[A]()

}
