package net.paploo.diestats.statistics.distribution

import java.util.Random

trait CDF[A] extends StrictlyOrderedDistribution[A] {

  def randomValue(implicit random: Random)

  def median: A = percentile(0.5)

  def percentile(p: Double): A

  def percentileOf(a: A): Double

}

object CDF {

  /**
    * Creates a CDF from the given PDF, using the given domain ordering.
    *
    * @param pdf The frequency distribution for the domain
    * @param ordering The ordering of the domain to use for accumulation
    * @tparam A The domain type
    * @return The resulting cumulative distribution function
    */
  def apply[A](pdf: PDF[A])(implicit ordering: Ordering[A]): CDF[A] = MapCDF(pdf)

  /**
    * TODO: Move into a generic helper.
    * @param pairs
    * @tparam A
    * @return
    */
  def accumulate[A](pairs: Iterable[(A, Double)]): Iterable[(A, Double)] = {
    type Memo = (Seq[(A, Double)], Double)
    val initialMemo: Memo = (Vector.empty[(A, Double)], 0.0)

    pairs.foldLeft[Memo](initialMemo){
      case (memo, pair) =>
        val sum = memo._2 + pair._2
        val cumulativePair = (pair._1, sum)
        (memo._1 :+ cumulativePair, sum)
    }._1
  }

}
