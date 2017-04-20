package net.paploo.diestats.statistics.pdf
import net.paploo.diestats.statistics.Probability
import net.paploo.diestats.statistics.cdf.CDF
import net.paploo.diestats.statistics.distribution.ConcreteDistributionCompanion
import net.paploo.diestats.statistics.domain.DomainOps

/**
  * Implementation of PDF built around immutable Map.
  * @param normalizedPairs A pre-normalized set of probabilities over the domain.
  * @tparam A
  */
private[pdf] final class PDFMap[A](normalizedPairs: Map[A, Probability]) extends PDF[A] {

  override def get(a: A): Option[Probability] = normalizedPairs.get(a)

  override def toMap: Map[A, Probability] = normalizedPairs

  override def toPDF: PDF[A] = this

  override def toCDF(implicit ordering: Ordering[A]): CDF[A] = CDF(this)
}

object PDFMap extends ConcreteDistributionCompanion[Probability, PDFMap] {

  override def empty[A]: PDFMap[A] = new PDFMap(Map.empty)

  override def buildFrom[A](pairs: Iterable[(A, Probability)]): PDFMap[A] = new PDFMap(pairs.toMap)

  private[pdf] def buildFromNormalized[A](pairs: Iterable[(A, Probability)]): PDFMap[A] = new PDFMap(pairs.toMap)

}
