package net.paploo.diestats.statistics.frequency
import java.util.concurrent.atomic.AtomicLong

import net.paploo.diestats.statistics.distribution.ConcreteDistributionCompanion

import scala.collection.mutable

/**
  * Thread safe mutable frequency accumulation buffer.
  * @tparam A The domain type.
  */
class AtomicFrequencyBuffer[A] extends FrequencyBuffer[A] {

  private[this] val frequencies: mutable.Map[A, AtomicLong] = mutable.Map.empty

  override def +=(pair: (A, Long)): FrequencyBuffer[A] = {
    getCounter(pair._1).addAndGet(pair._2)
    this
  }

  override def ++=(pairs: TraversableOnce[(A, Long)]): FrequencyBuffer[A] = {
    pairs.foreach(pair => this += pair)
    this
  }

  override def +(pair: (A, Long)): Frequency[A] = toFrequency + pair

  override def ++(pairs: TraversableOnce[(A, Long)]): Frequency[A] = toFrequency ++ pairs

  override def count: Long = frequencies.values.foldLeft(0L)(_.longValue + _.longValue)

  override def get(a: A): Option[Long] = frequencies.get(a).map(_.longValue)

  override def domain(implicit ord: Ordering[A]): Seq[A] = frequencies.keys.toSeq.sorted(ord)

  override def copy: FrequencyBuffer[A] = AtomicFrequencyBuffer.buildFrom(this.toMap)

  override def toMap: Map[A, Long] = toFrequency.toMap

  /**
    * Returns an immutable copy of this frequency buffer.
    */
  override def toFrequency: Frequency[A] = Frequency.buildFrom(this.toMap)

  /**
    * Get the counter, creating it if necessary, in a thread safe way.
    */
  private[this] def getCounter(a: A): AtomicLong = this.synchronized {
    frequencies.getOrElseUpdate(a, new AtomicLong(0L))
  }

}

object AtomicFrequencyBuffer extends ConcreteDistributionCompanion[Long, AtomicFrequencyBuffer] {

  override def empty[A]: AtomicFrequencyBuffer[A] = new AtomicFrequencyBuffer[A]()

  override def buildFrom[A](pairs: TraversableOnce[(A, Long)]): AtomicFrequencyBuffer[A] = {
    val buf = empty[A]
    buf ++= pairs
    buf
  }

}
