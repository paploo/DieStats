package net.paploo.diestats.statistics.frequency

import net.paploo.diestats.statistics.distribution.{Distribution, DistributionCompanion, DistributionStatistics, NumericDistributionStatistics, StatisticalDistribution}
import net.paploo.diestats.statistics.probabilitydistribution.{ProbabilityDistribution, ProbabilityDistributionable}

import scala.language.higherKinds

/**
  * Frequency base trait, defining all methods that can be used by both immutable and mutable subclasses.
  * @tparam A The domain type.
  */
trait Frequency[A] extends Distribution[A, Long] with Frequenciable[A] with ProbabilityDistributionable[A] with StatisticalDistribution[A, Long] {

  /**
    * Returns a new Frequency with the given count added to the pair.
    * @param pair
    * @return
    */
  def +(pair: (A, Long)): Frequency[A]

  /**
    * Returns a new Frequency with the given frequencies added in.
    * @param pairs
    */
  def ++(pairs: Iterable[(A, Long)]): Frequency[A]

  /**
    * Appends a value with a count of 1.
    * @param value
    * @return
    */
  def :+(value: A): Frequency[A] = this + ((value, 1L))

  /**
    * Appends the list of values, with a frequency count for each instance in the list.
    * @param values
    * @return
    */
  def :++(values: Iterable[A]): Frequency[A] = this ++ values.map((_,1L))

  /**
    * Give the sum total of counts across the domains.
    * @return
    */
  def sum: Long

  override def toFrequency: Frequency[A] = this

  override def toProbabilityDistribution: ProbabilityDistribution[A] = ProbabilityDistribution(this)

  override def toStatistics(implicit ord: Ordering[A]): DistributionStatistics[A, Long] =
    DistributionStatistics.fromDistributionPairs(toSeq)

  override def toNumericalDomainStatistics(implicit num: Numeric[A]): NumericDistributionStatistics[A, Long] =
    DistributionStatistics.fromNumericDistributionPairs(toSeq)
}

object Frequency extends FrequencyCompanion[Frequency] {

  override def empty[A]: Frequency[A] = FrequencyMap.empty

  override def buildFrom[A](pairs: Iterable[(A, Long)]): Frequency[A] = FrequencyMap.buildFrom(pairs)

  override def buildFromValues[A](values: Iterable[A]): Frequency[A] = FrequencyMap.buildFromValues(values)

}

/**
  * A common trait for all values that can be transformed into a Frequency.
  */
trait Frequenciable[A] {

  def toFrequency: Frequency[A]

}

trait FrequencyCompanion[Repr[_]] extends DistributionCompanion[Long, Repr] {

  def fromValues[A](values: A*): Repr[A] = buildFromValues(values)

  def buildFromValues[A](values: Iterable[A]): Repr[A]

  def emptyWithDomain[A](domain: A*): Repr[A] = buildWithDomain(domain)

  def buildWithDomain[A](domain: Iterable[A]): Repr[A] = buildFrom(domain.map(a => (a, 0L)))

}