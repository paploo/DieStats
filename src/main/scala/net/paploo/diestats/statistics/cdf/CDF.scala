package net.paploo.diestats.statistics.cdf

import net.paploo.diestats.statistics.distribution.ProbabilityDistribution
import net.paploo.diestats.statistics.pdf.PDFAble

/**
  * Cumulative distribution function over a domain A.
  * @tparam A The domain type.
  */
trait CDF[A] extends ProbabilityDistribution[A] with CDFable[A] with PDFAble[A] {

  // TODO: Think about the domain requirements!
  // CDF's need a predictably ordered domain, so that we know how to integrate from a PDF.

  def randomValue(implicit rand: RandomNumberGenerator): Double

  trait RandomNumberGenerator {
    def doubleValue: Double
  }

}

/**
  * A common trait for all values that can be transformed into a CDF.
  */
trait CDFable[A] {

  def toCDF: CDF[A]

}
