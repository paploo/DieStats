package net.paploo.diestats.statistics.distribution

import net.paploo.diestats.statistics.domain.DomainOperations
import net.paploo.diestats.util.OrderingSupport

object MapPDF {

  def apply[A](map: Map[A, Double]): MapPDF[A] = new MapPDF[A](PDF.normalizeMap(map))

}

private[distribution] class MapPDF[A](preNormalizedMap: Map[A, Double]) extends PDF[A] {

  val toMap: Map[A, Double] = preNormalizedMap

  override def get(a: A): Option[Double] = toMap.get(a)

  override lazy val domainSet: Set[A] = toMap.keys.toSet

  override def domain(implicit ord: Ordering[A]): Seq[A] =
    domainSet.toSeq.sorted

  override def pairs(implicit ord: Ordering[A]): Iterable[(A, Double)] =
    toMap.toSeq.sortBy(_._1)

  override def convolve(that: PDF[A])(implicit domainOps: DomainOperations[A]): PDF[A] = {
    val rawPairs = for {
      x <- this.domain(OrderingSupport.nullOrdering)
      y <- that.domain(OrderingSupport.nullOrdering)
    } yield (domainOps.concat(x, y), this(x) * that(y))

    val reducedPairs = rawPairs.groupBy(_._1).map {
      case (k,v) => k -> v.foldLeft(0.0)(_ + _._2)
    }

    new MapPDF(reducedPairs)
  }

  override def toCDF(implicit ordering: Ordering[A]): CDF[A] = ???

}
