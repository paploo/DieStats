package net.paploo.diestats.statistics.cdf

import net.paploo.diestats.statistics.Probability
import net.paploo.diestats.statistics.distribution.ProbabilityDistribution
import net.paploo.diestats.statistics.domain.DomainOps
import net.paploo.diestats.statistics.pdf.PDFAble

/**
  * Cumulative distribution function over a domain A.
  *
  * CDFs have an implied domain ordering
  *
  * @tparam A The domain type.
  */
trait CDF[A] extends ProbabilityDistribution[A] with CDFable[A] with PDFAble[A] {

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

  def randomValue(implicit rand: RandomNumberGenerator): Double

  trait RandomNumberGenerator {
    def doubleValue: Double
  }

}

/**
  * A common trait for all values that can be transformed into a CDF.
  */
trait CDFable[A] {

  def toCDF(dops: DomainOps[A]): CDF[A]

}
