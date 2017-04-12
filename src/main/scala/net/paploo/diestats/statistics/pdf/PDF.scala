package net.paploo.diestats.statistics.pdf

import net.paploo.diestats.statistics.Probability
import net.paploo.diestats.statistics.cdf.CDFable
import net.paploo.diestats.statistics.distribution.Distribution
import net.paploo.diestats.statistics.domain.DomainOps

/**
  * Probabilitiy Distribution Function
  */
trait PDF[A] extends Distribution[A, Probability] with PDFAble[A] with CDFable[A] {

  def convolve(that: PDF[A])(implicit dops: DomainOps[A]): PDF[A]

}

/**
  * A common trait for all values that can be transformed into a PDF.
  */
trait PDFAble[A] {

  def toPDF: PDF[A]

}
