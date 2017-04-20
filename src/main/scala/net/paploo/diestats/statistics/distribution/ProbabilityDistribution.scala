package net.paploo.diestats.statistics.distribution

import net.paploo.diestats.statistics.Probability
import net.paploo.diestats.statistics.cdf.CDFAble
import net.paploo.diestats.statistics.pdf.PDFAble

trait ProbabilityDistribution[A] extends Distribution[A, Probability] {

}
