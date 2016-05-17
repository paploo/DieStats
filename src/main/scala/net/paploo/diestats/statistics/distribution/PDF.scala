package net.paploo.diestats.statistics.distribution

trait PDF[A] extends Distribution[A] {

  def convolve[A1 >: A](other: PDF[A1]): PDF[A1]

  def toCDF: CDF[A]

}

object PDF {

  /**
    * Creates a PDF from the ordered sequence of pairs of counts.
    *
    * Domain ordering is dictated by the order of the pairs.
    *
    * @param pairs The count of values for the given domain
    * @tparam A The domain type
    * @throws IllegalArgumentException if any count is negative.
    */
  def fromCounts[A](pairs: Seq[(A, Long)]): PDF[A] = {
    val sum = pairs.map(_._2).sum.toDouble
    val normalizedPairs = pairs.map { case (a,c) => (a, c.toDouble/sum)}
    apply(normalizedPairs)
  }

  /**
    * Creates a PDF from the ordered sequence of pairs of fractional counts.
    *
    * Domain ordering is dictated by the order of the pairs.
    *
    * This re-normalizes if the probabilities do not sum to 1.0. The special case
    * are pairs from PDFs, which are unchanged under normalization.
    *
    * @param pairs The count of values for the given domain
    * @tparam A The domain type
    * @throws IllegalArgumentException if any count is negative.
    */
  def apply[A](pairs: Seq[(A, Double)]): PDF[A] = ???

}