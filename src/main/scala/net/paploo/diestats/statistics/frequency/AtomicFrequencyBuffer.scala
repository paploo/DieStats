package net.paploo.diestats.statistics.frequency
import java.util.concurrent.atomic.AtomicLong

import net.paploo.diestats.statistics.distribution.PDF

import scala.collection.mutable

class AtomicFrequencyBuffer[A](override val domain: Iterable[A], unknownValueHandler: A => Option[A]) extends Frequency[A] {

  private[this] val buffer: mutable.Map[A, AtomicLong] = initializeBuffer(domain)

  override def apply(a: A): Long = buffer(a).get

  override def add(a: A, delta: Long): Frequency[A] = {
    if (buffer.isDefinedAt(a)) buffer(a).addAndGet(delta)
    else unknownValueHandler(a).map(buffer(_).addAndGet(delta))

    this
  }

  override def toPDF: PDF[A] = ???

  override def counts: Long = buffer.values.foldLeft(0L)(_ + _.get)

  private def initializeBuffer(domain: Iterable[A]): mutable.Map[A, AtomicLong] = {
    val map = mutable.HashMap.empty[A, AtomicLong]
    domain.foreach(map(_) = new AtomicLong())
    map
  }
}
