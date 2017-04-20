package net.paploo.diestats.statistics.cdf

import net.paploo.diestats.statistics.Probability
import net.paploo.diestats.statistics.distribution.ProbabilityDistribution
import net.paploo.diestats.statistics.domain.DomainOps
import net.paploo.diestats.statistics.pdf.PDF
import net.paploo.diestats.statistics.util.RandomGenerator

/**
  * Cumulative distribution function over a domain A.
  *
  * CDFs have an implied domain ordering
  *
  * @tparam A The domain type.
  */
trait CDF[A] extends ProbabilityDistribution[A] {

  /**
    * While a PDF has no domain ordering, PDFs represent an integration, which requires a definitive ordering.
    */
  def domainOrdering: Ordering[A]

  /**
    * Gets the domain list, sorted according to the CDF's ordering.
    */
  def domain: Seq[A] = domain(domainOrdering)

  /**
    * Gets the pairs, sorted according to the CDF's ordering.
    */
  def pairs: Seq[(A, Probability)] = pairs(domainOrdering)

  /**
    * Returns a random domain value, distributed according to the CDF.
    */
  def randomValue(implicit rand: RandomGenerator): A

}

object CDF {

  def empty[A](implicit ordering: Ordering[A]): CDF[A] = CDFMap.empty

  def apply[A](pdf: PDF[A])(implicit ordering: Ordering[A]): CDF[A] = {
    val accumulator = pdf.pairs.foldLeft(Accumulator[A]())(_ + _)
    CDFMap.buildFromValidatedPairs(accumulator.pairs)
  }

  /**
    * Private helper class to aid in accumulating PDF values into the CDF.
    */
  private[this] case class Accumulator[A](pairs: Seq[(A, Probability)] = Seq.empty,
                                          runningSum: Probability = Probability.zero) {
    def +(pair: (A, Probability)): Accumulator[A] = {
      val newRunningSum = this.runningSum |+| pair._2 //In case of rounding errors, we use Probability.truncated
      val newPair = (pair._1, newRunningSum)
      Accumulator(pairs :+ newPair, newRunningSum)
    }
  }

}

/**
  * A common trait for all values that can be transformed into a CDF.
  */
trait CDFAble[A] {

  def toCDF(implicit ordering: Ordering[A]): CDF[A]

}
