package net.paploo.diestats.statistics.distribution

import java.util.Random

object MapCDF {

  def apply[A](pdf: PDF[A])(implicit ordering: Ordering[A]): MapCDF[A] = {
    val pairs = CDF.accumulate(pdf.pairs(ordering))
    //TODO: I already have nice sorted pairs, maybe I should use these—the only downside being that random access is slow.
    new MapCDF(pairs.toMap)(ordering)
  }

}

private[distribution] class MapCDF[A](map: Map[A, Double])(implicit val ordering: Ordering[A]) extends CDF[A] {

  val toMap: Map[A, Double] = map

  override def get(a: A): Option[Double] = toMap.get(a)

  override lazy val domainSet: Set[A] = toMap.keys.toSet

  override lazy val domain: Seq[A] = super.domain

  override lazy val pairs: Iterable[(A, Double)] = toMap.toSeq.sortBy(_._1)(ordering)

  override def percentileOf(a: A): Double = ???

  override def percentile(p: Double): A = ???

  override def randomValue(implicit random: Random): Unit = ???
}
