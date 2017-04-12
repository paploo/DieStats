package net.paploo.diestats.statistics.frequency

import net.paploo.diestats.statistics.distribution.Distribution
import net.paploo.diestats.statistics.pdf.PDFAble

/**
  * Frequency base trait, defining all methods that can be used by both immutable and mutable subclasses.
   * @tparam A The domain type.
  */
trait Frequency[A] extends Distribution[A, Long] with Frequenciable[A] with PDFAble[A] {

  def +(pair: (A, Long)): Frequency[A]

  def ++(that: Frequency[A])

}

/**
  * A common trait for all values that can be transformed into a Frequency.
  */
trait Frequenciable[A] {

  def toFrequency: Frequency[A]

}
