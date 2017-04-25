package net.paploo.diestats.test

import net.paploo.diestats.statistics.util.Probability
import org.scalatest.{BeforeAndAfterEach, FunSpec, Matchers}

trait SpecTest extends FunSpec with Matchers with BeforeAndAfterEach {

  // The acceptable error to use in double floating point precision numerical "equality" tests.
  val ε = 1e-12

}

/**
  * Wolfram Alpha: BinomialDistribution[4, 1/4]
  */
trait TestBinomialDistributionFourTrialsOfOneOverFour {

  val probability: Probability = Probability(1/4)

  val trials: Long = 4

  def domain: Seq[Long] = Seq(0L, 1L, 2L, 3L ,4L)

  def pairs: Seq[(Long, Probability)] = Seq(
    0L -> Probability(81, 256),
    1L -> Probability(108, 256),
    2L -> Probability(54, 256),
    3L -> Probability(12, 256),
    4L -> Probability(1, 256)
  )

  def cumulativePairs: Seq[(Long, Probability)] = Seq(
    0L -> Probability(81, 256),            //  81/256 = 0.31640625
    1L -> Probability(81+108, 256),        // 189/256 = 0.73828125
    2L -> Probability(81+108+54, 256),     // 243/256 = 0.94921875
    3L -> Probability(81+108+54+12, 256),  // 255/256 = 0.99609375
    4L -> Probability(81+108+54+12+1, 256) // 256/256 = 1.00000000
  )

  val median: Long = 1L

  val modes: Seq[Long] = Seq(1L)

  val mean: Double = 1.0

  val variance: Double = 3.0 / 4.0

  val stdDev: Double = Math.sqrt(variance)

  val skewness: Double = 1.0 / Math.sqrt(3.0)

  val kurtosis: Double = 17.0 / 6.0

}