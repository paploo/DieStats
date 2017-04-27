package net.paploo.diestats.statistics.probabilitydistribution

import net.paploo.diestats.statistics.distribution.{DistributionCompanion, Distribution, DistributionStatistics, NumericDistributionStatistics, StatisticalDistribution}
import net.paploo.diestats.statistics.frequency.Frequency
import net.paploo.diestats.statistics.util.{Monoid, Probability}

import scala.collection.mutable

/**
  * Probabilitiy Distribution
  */
trait ProbabilityDistribution[A] extends Distribution[A, Probability] with StatisticalDistribution[A, Probability] with ProbabilityDistributionable[A] {

  def convolve(that: ProbabilityDistribution[A])(implicit monoid: Monoid[A]): ProbabilityDistribution[A] = {
    // For both better memory usage and speed, use mutable.Map as a buffer, and then make immutable.
    val buffer = mutable.Map.empty[A, Probability]
    for {
      (xa, xp) <- this.toSeq
      (ya, yp) <- that.toSeq
    } {
      val key = monoid.concat(xa, ya)
      val value = buffer.getOrElseUpdate(key, Probability.zero) + (xp * yp)
      buffer += key -> value
    }
    ProbabilityDistribution.buildFromNormalized(buffer) //Note: This is technically mutable, but we return as an immutable interface; we could use toMap to be safer, but then we'd be making an unecesssary copy to enforce immutability.
  }

  override def toStatistics(implicit ord: Ordering[A]): DistributionStatistics[A, Probability] =
    DistributionStatistics.fromDistributionPairs(toSeq)

  override def toNumericalDomainStatistics(implicit num: Numeric[A]): NumericDistributionStatistics[A, Probability] =
    DistributionStatistics.fromNumericDistributionPairs(toSeq)
}

object ProbabilityDistribution extends DistributionCompanion[Probability, ProbabilityDistribution] {

  override def empty[A]: ProbabilityDistribution[A] = ProbabilityDistributionMap.empty

  def apply[A](freq: Frequency[A]): ProbabilityDistribution[A] = {
    val sum = freq.sum
    val pairs = freq.toMap.mapValues(m => Probability(m, sum))
    ProbabilityDistribution.buildFrom(pairs)
  }

  /**
    * Given the pairs, create a ProbabilityDistribution.
    *
    * This normalizes any unnormalized distributions.
    */
  override def buildFrom[A](pairs: Iterable[(A, Probability)]): ProbabilityDistribution[A] = {
    val normalizedPairs = Probability.normalizePairs(pairs)
    ProbabilityDistributionMap.buildFrom(normalizedPairs)
  }

  /**
    * Create a ProbabilityDistribution from guaranteed pre-normalized pairs, skipping any validation of normalization.
    *
    * This is used with caution, to get performance increases in cases where we know we can trust the source.
    *
    * Visibility is kept to within the statistics package, where we can trust the caller has pre-normalized.
    */
  private[probabilitydistribution] def buildFromNormalized[A](pairs: Iterable[(A, Probability)]): ProbabilityDistribution[A] =
    ProbabilityDistributionMap.buildFromNormalized(pairs)

}

/**
  * A common trait for all values that can be transformed into a ProbabilityDistribution.
  */
trait ProbabilityDistributionable[A] {

  def toProbabilityDistribution: ProbabilityDistribution[A]

}
