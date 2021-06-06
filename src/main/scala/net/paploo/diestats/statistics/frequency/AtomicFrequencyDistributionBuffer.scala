package net.paploo.diestats.statistics.frequency
import java.util.concurrent.atomic.AtomicLong

import scala.collection.mutable

/**
  * Thread safe mutable frequency accumulation buffer.
  * @tparam A The domain type.
  */
private[frequency] final class AtomicFrequencyDistributionBuffer[A] extends FrequencyDistributionBuffer[A] {

  private[this] val frequencies: mutable.Map[A, AtomicLong] = mutable.Map.empty

  override def get(a: A): Option[Long] = frequencies.get(a).map(_.longValue)

  override def sum: Long = frequencies.values.foldLeft(0L)(_.longValue + _.longValue)

  override def size: Int = frequencies.size

  override def domain(implicit ord: Ordering[A]): Seq[A] = frequencies.keys.toSeq.sorted(ord)

  override def +=(pair: (A, Long)): FrequencyDistributionBuffer[A] = {
    getCounter(pair._1).addAndGet(pair._2)
    this
  }

  override def ++=(pairs: Iterable[(A, Long)]): FrequencyDistributionBuffer[A] = {
    pairs.foreach(pair => this += pair)
    this
  }

  override def +(pair: (A, Long)): FrequencyDistribution[A] = toFrequency + pair

  override def ++(pairs: Iterable[(A, Long)]): FrequencyDistribution[A] = toFrequency ++ pairs

  override def copy: FrequencyDistributionBuffer[A] = AtomicFrequencyDistributionBuffer.buildFrom(this.toMap)

  override def toMap: Map[A, Long] = frequencies.view.mapValues(_.longValue()).toMap

  /**
    * Returns an immutable copy of this frequency buffer.
    */
  override def toFrequency: FrequencyDistribution[A] = FrequencyDistribution.buildFrom(this.toMap)

  /**
    * Get the counter, creating it if necessary, in a thread safe way.
    */
  private[this] def getCounter(a: A): AtomicLong = this.synchronized {
    frequencies.getOrElseUpdate(a, new AtomicLong(0L))
  }

  override def toString(): String = s"FrequencyMap(${this.toMap.mkString(", ")})"

}

object AtomicFrequencyDistributionBuffer extends FrequencyDistributionCompanion[AtomicFrequencyDistributionBuffer] {

  override def empty[A]: AtomicFrequencyDistributionBuffer[A] = new AtomicFrequencyDistributionBuffer[A]()

  override def buildFrom[A](pairs: Iterable[(A, Long)]): AtomicFrequencyDistributionBuffer[A] = {
    val buf = empty[A]
    buf ++= pairs
    buf
  }

  override def buildFromValues[A](values: Iterable[A]): AtomicFrequencyDistributionBuffer[A] = {
    val buffer = AtomicFrequencyDistributionBuffer.empty[A]
    values.foreach(buffer.append)
    buffer
  }

}
