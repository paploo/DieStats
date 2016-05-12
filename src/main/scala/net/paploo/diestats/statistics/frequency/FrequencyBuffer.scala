package net.paploo.diestats.statistics.frequency

import net.paploo.diestats.statistics.distribution.PDF

import scala.collection.mutable

class FrequencyBuffer[A](override val domain: Iterable[A], unknownValueHandler: A => Option[A]) extends Frequency[A] {

  private[this] val buffer: mutable.Map[A, Long] = initializeBuffer(domain)


  override def apply(a: A): Long = buffer(a)

  override def add(a: A, delta: Long): Frequency[A] = {
    if (buffer.isDefinedAt(a)) buffer(a) += delta
    else unknownValueHandler(a).map(buffer(_) += delta)

    this
  }

  override def counts: Long = buffer.values.sum

  override def toPDF: PDF[A] = ???

  private def initializeBuffer(domain: Iterable[A]): mutable.Map[A, Long] = {
    val map = mutable.HashMap.empty[A, Long]
    domain.foreach(map(_) = 0)
    map
  }
}

object FrequencyBuffer {

  def apply[A](domain: Iterable[A]): FrequencyBuffer[A] = apply(domain, _ => None)

  def apply[A](domain: Iterable[A], unknownValueHandler: A => Option[A]): FrequencyBuffer[A] = new FrequencyBuffer(domain, unknownValueHandler)

}