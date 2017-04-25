package net.paploo.diestats.statistics.distribution

import net.paploo.diestats.statistics.util.{Probability, RandomGenerator}

/**
  * Computed Statistics over a frequency distribution.
  *
  * As part of computation, domain ordering and frequency ordering
  * are solidified. For example, percentile calculation requires
  * sorting and accumulating, which are both expensive operations
  * (and even domain min and max require sorting).
  *
  * Technically, this could be divided into intermediate traits for
  * ordered domains (e.g. min, max, and domain), and ordered ranges
  * (e.g. modes)
  *
  * Technically, we could have an intermediate trait for an ordered range
  * (that is non-numeric, and hence not a frequency), that contains a subset
  * of these computations, but there is little need for this normalization.
  *
  * @tparam A The domain type
  * @tparam N The frequency type
  */
/*
  * TODO: This took the example of, say, a SortedMap, and stores the ordering, however this feels weird, since typeclasses are usually applied at the call site;
  * One problem is that we've conflated generation of the statistics with caching of the values. We may want to break
  * it up, however problems arise when we think about computing percentiles, which are only performant when we cache the
  * accumulations over a Numeric[N] and a domain Ordering[A].
  * A solution would be to separate out the static precomputed bits (like min, max, mean, kurtosis) from the
  * functions (like percentile calculation) so that percentiles are done against a specific ordering/numeric
  * while everything else is precomputed static data. What this really means, though, is that computation will
  * likely get moved into a factory class which looks just like the values here, at which point we might as
  * well keep them together.
 */
trait DistributionStatistics[A, N] {

  implicit def domainOrdering: Ordering[A]

  implicit def frequencyNumeric: Numeric[N]

  implicit def frequencyOrdering: Ordering[N] = frequencyNumeric

  /**
    * The pairs of frequency/probability vs. domain.
    * @return
    */
  def sortedPairs: Seq[(A, N)]

  /**
    * The cumulative distribution pairs.
    *
    * Note that the cumulation happens on the numeric type of the
    * distribution, and is NOT normalized as a Probability.
    * @return
    */
  def cumulativePairs: Seq[(A, N)]

  /**
    * The domain, ordered according to domainOrdering.
    * @return
    */
  def domain: Seq[A]

  def min: A

  def max: A

  /**
    * The sum of the frequency counts
    * @return
    */
  def sum: N

  /**
    * All domain values whose frequency corresponds to the highest frequency.
    * @return
    */
  def modes: Seq[A]

  def median: A

  def percentile(p: Probability): A

  def selectRandom(implicit rand: RandomGenerator): A = percentile(rand.nextProbability())
}

/**
  * In some cases, the domain can be considered numeric, and extra statistics can
  * then be calculated.
  * @tparam A The domain type
  * @tparam N The frequency type
  */
trait NumericDistributionStatistics[A, N] extends DistributionStatistics[A, N] {

  implicit def domainNumeric: Numeric[A]

  /**
    * Returns the mean.
    *
    * Mean is only calculable when the domain and frequencies are treated as fractional values.
    * @return
    */
  def mean: Double

  def variance: Double

  def stdDev: Double

  def skewness: Double

  def kurtosis: Double

}

object DistributionStatistics {

  private[statistics] def fromDistributionPairs[A, N](unsortedPairs: Iterable[(A, N)])
                                                     (implicit domainOrdering: Ordering[A], frequencyNumeric: Numeric[N]): DistributionStatistics[A, N] =
    new DefaultDistributionStatistics[A, N](unsortedPairs)

  private[statistics] def fromNumericDistributionPairs[A, N](unsortedPairs: Iterable[(A, N)])
                                                            (implicit domainNumeric: Numeric[A], frequencyNumeric: Numeric[N]): NumericDistributionStatistics[A, N] =
    new DefaultNumericDistributionStatistics[A, N](unsortedPairs)

  private[this] class DefaultDistributionStatistics[A, N](unsortedPairs: Iterable[(A, N)])(implicit override val domainOrdering: Ordering[A], override val frequencyNumeric: Numeric[N])
    extends DistributionStatistics[A, N] {

    override val sortedPairs: Seq[(A, N)] = unsortedPairs.toSeq.sortBy(_._1)

    override lazy val cumulativePairs: Seq[(A, N)] = CumulativePairs.accumulate(unsortedPairs)

    override val domain: Seq[A] = sortedPairs.map(_._1)

    override val min: A = domain.min

    override val max: A = domain.max

    override val sum: N = sortedPairs.foldLeft(frequencyNumeric.zero) {
      (s, pair) => frequencyNumeric.plus(s, pair._2)
    }

    override lazy val modes: Seq[A] = {
      val maxValue = sortedPairs.map(_._2).max(frequencyOrdering)
      sortedPairs.flatMap { pair =>
        if (frequencyOrdering.equiv(pair._2, maxValue)) Seq(pair._1)
        else Seq.empty
      }
    }

    override lazy val median: A = percentile(Probability(1L, 2L))

    override def percentile(p: Probability): A = cumulativePairs.find { pair =>
      // TODO: Ideally, we'd convert N to Probability, especially when N is integral
      val numerator: BigDecimal = frequencyNumeric.toDouble(pair._2)
      val denominator: BigDecimal = frequencyNumeric.toDouble(sum)
      val normalizedValue = numerator / denominator
      p.toBigDecimal <= normalizedValue
    }.get._1

  }

  private[this] class DefaultNumericDistributionStatistics[A, N](unsortedPairs: Iterable[(A, N)])(implicit override val domainNumeric: Numeric[A], frequencyNumeric: Numeric[N])
    extends DefaultDistributionStatistics[A, N](unsortedPairs)(domainNumeric, frequencyNumeric) with NumericDistributionStatistics[A, N] {

    override lazy val mean: Double = {
      val frequencySumDouble = frequencyNumeric.toDouble(sum)
      // calculate the weighted values, but normalize by denominator sooner than later
      sortedPairs.foldLeft(0.0){ (terms, pair) =>
        val weight = frequencyNumeric.toDouble(pair._2) / frequencySumDouble
        val domainDouble = domainNumeric.toDouble(pair._1)
        terms + (weight * domainDouble)
      }
    }

    override def variance: Double = ???

    override def stdDev: Double = Math.sqrt(variance)

    override def skewness: Double = ???

    override def kurtosis: Double = ???
  }

  object CumulativePairs {
    /**
      * Accumulates the given list of pairs according the provided domain ordering;
      * the result is also properly ordered.
      *
      * When seeded with pairs from a ProbabilityDistribution, a cumulative distribution is returned.
      */
    def accumulate[A, N](pairs: Iterable[(A, N)])(implicit ordA: Ordering[A], numN: Numeric[N]): Seq[(A, N)] =
      pairs.toSeq.sorted.foldLeft(Memo.empty[A,N])(_ + _).cumulativePairs

    private[this] case class Memo[A, N](cumulativePairs: Seq[(A, N)], runningSum: N)(implicit numN: Numeric[N]) {
      def +(pair: (A,N)): Memo[A, N] = {
        val newRunningSum = numN.plus(runningSum, pair._2) //TODO: For Probability, rounding errors could cause this to be 1+ε and fail to construct.
        Memo(cumulativePairs :+ pair.copy(_2 = newRunningSum), newRunningSum)
      }
    }

    private[this] object Memo {
      def empty[A, N](implicit numN: Numeric[N]): Memo[A, N] = Memo(Vector.empty, numN.zero)
    }

  }

}

trait StatisticalDistribution[A, N] {

  def statistics(implicit ord: Ordering[A]): DistributionStatistics[A, N]

  def numericalDomainStatistics(implicit num: Numeric[A]): NumericDistributionStatistics[A, N]

}