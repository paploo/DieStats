package net.paploo.diestats.statistics.cdf

import net.paploo.diestats.statistics.Probability
import net.paploo.diestats.statistics.distribution.Distribution
import net.paploo.diestats.statistics.pdf.{PDF, PDFAble}

/**
  * Cumulative distribution function over a domain A.
  * @tparam A The domain type.
  */
trait CDF[A] extends Distribution[A, Probability] with CDFable[A] with PDFAble[A]

/**
  * A common trait for all values that can be transformed into a CDF.
  */
trait CDFable[A] {

  def toCDF: CDF[A]

}
