package net.paploo.diestats.statistics.distribution

import java.util.Random

trait CDF[A] extends Distribution[A] {

  def randomValue(implicit random: Random)

  def median: A = percentile(0.5)

  def percentile(p: Double): A

  def percentileOf(a: A): Double

}

object CDF {

  /**
    * Creates a CDF with the given pairs of cumulative probabilities.
    *
    * @param pairs
    * @tparam A
    * @throws IllegalArgumentException if not monotonic.
    * @throws IllegalArgumentException if any value is below 0.0 or above 1.0
    */
  def apply[A](pairs: Seq[(A, Double)]): CDF[A] = ???

}
