package net.paploo.diestats.statistics.distribution

import net.paploo.diestats.statistics.pdf.PDFAble
import net.paploo.diestats.statistics.util.{StatisticalDistribution, Probability}

trait ProbabilityDistribution[A] extends Distribution[A, Probability] with StatisticalDistribution[A, Probability] with PDFAble[A]