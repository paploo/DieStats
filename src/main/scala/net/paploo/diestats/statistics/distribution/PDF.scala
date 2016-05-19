package net.paploo.diestats.statistics.distribution

import net.paploo.diestats.statistics.domain.DomainOperations

trait PDF[A] extends Distribution[A] {

  def convolve(that: PDF[A])(implicit domainOps: DomainOperations[A]): PDF[A]

  def toCDF: CDF[A]

}

object PDF {

  /**
    * Creates a PDF from the ordered sequence of pairs of counts.
    *
    * @param map The count of values for each element of the given domain
    * @tparam A The domain type
    * @throws IllegalArgumentException if any count is negative.
    */
  def fromCounts[A](map: Map[A, Long]): PDF[A] = apply(map.mapValues(_.toDouble))

  /**
    * Creates a PDF from the ordered sequence of pairs of fractional counts.
    *
    * This re-normalizes if the probabilities do not sum to 1.0. The special case
    * are pairs from PDFs, which are unchanged under normalization.
    *
    * @param map The count of values for each element of the given domain
    * @tparam A The domain type
    * @throws IllegalArgumentException if any count is negative.
    */
  def apply[A](map: Map[A, Double]): PDF[A] = MapPDF.apply(map)


  /**
    *
    * @param map
    * @param num
    * @tparam A
    * @tparam N
    * @return
    */
  private[distribution] def normalizeMap[A, N](map: Map[A, N])(implicit num: Numeric[N]): Map[A, Double] = {
    val sum = num.toDouble(map.values.sum(num))

    map.mapValues {n =>
      if (num.lt(n, num.zero)) throw new IllegalArgumentException("Input map has negative values.")
      else num.toDouble(n) / sum
    }
  }

}