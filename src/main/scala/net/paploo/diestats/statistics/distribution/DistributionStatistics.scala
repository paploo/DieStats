package net.paploo.diestats.statistics.distribution

import net.paploo.diestats.statistics.util.{FrequencyNumeric, Probability, RandomGenerator}

/**
  * Computed Statistics over a frequency distribution, closed
  * over a concrete Ordering[A] and Numeric[N].
  *
  * As part of computation, domain ordering and frequency ordering
  * are solidified. For example, percentile calculation requires
  * sorting and accumulating (which are both expensive operations),
  * and even domain min and max require pre-sorting.
  *
  * Technically, this could be divided into intermediate traits for
  * ordered domains (e.g. min, max, and domain), and ordered ranges
  * (e.g. modes).
  *
  * @tparam A The domain type
  * @tparam N The frequency type
  */
trait DistributionStatistics[A, N] {

  implicit def domainOrdering: Ordering[A]

  implicit def frequencyNumeric: FrequencyNumeric[N]

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
  * then be calculated over the Numeric[A].
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
                                                     (implicit domainOrdering: Ordering[A], frequencyNumeric: FrequencyNumeric[N]): DistributionStatistics[A, N] =
    new DefaultDistributionStatistics[A, N](unsortedPairs)

  private[statistics] def fromNumericDistributionPairs[A, N](unsortedPairs: Iterable[(A, N)])
                                                            (implicit domainNumeric: Numeric[A], frequencyNumeric: FrequencyNumeric[N]): NumericDistributionStatistics[A, N] =
    new DefaultNumericDistributionStatistics[A, N](unsortedPairs)

  private[this] class DefaultDistributionStatistics[A, N](unsortedPairs: Iterable[(A, N)])(implicit override val domainOrdering: Ordering[A], override val frequencyNumeric: FrequencyNumeric[N])
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
      p <= frequencyNumeric.toProbability(pair._2, sum)
    }.get._1

  }

  private[this] class DefaultNumericDistributionStatistics[A, N](unsortedPairs: Iterable[(A, N)])(implicit override val domainNumeric: Numeric[A], frequencyNumeric: FrequencyNumeric[N])
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

    override lazy val variance: Double = cumulant(2)

    override lazy val stdDev: Double = Math.sqrt(variance)

    override lazy val skewness: Double = cumulant(3) / Math.pow(stdDev, 3)

    override lazy val kurtosis: Double = cumulant(4) / Math.pow(stdDev, 4)

    /**
      * Calculate the k-th cumulant.
      *
      * The cumulant is
      *   Expectation[((X - mu)/sigma)^k],
      * which for a list of values is
      *   (1/N) * Sum[(xi - mu)^k, {i,1,N}].
      * Taking into account that we have frequencies (weights), and
      * distributing the coefficient, we get:
      *   Sum[(wi/N) * (xi - mu)^k, i]
      */
    private[this] def cumulant(k: Int) =  {
      val n: Double = frequencyNumeric.toDouble(sum)
      unsortedPairs.foldLeft(0.0) { (sum, pair) =>
        val weight = frequencyNumeric.toDouble(pair._2) / n
        val term = weight * Math.pow(domainNumeric.toDouble(pair._1) - mean, k)
        sum + term
      }
    }
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