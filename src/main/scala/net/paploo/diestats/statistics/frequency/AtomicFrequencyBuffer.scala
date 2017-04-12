package net.paploo.diestats.statistics.frequency
import java.util.concurrent.atomic.AtomicLong

import net.paploo.diestats.statistics.domain.DomainOps
import net.paploo.diestats.statistics.pdf.PDF

import scala.collection.mutable

/**
  * Thread safe mutable frequency accumulation buffer.
  * @tparam A The domain type.
  */
class AtomicFrequencyBuffer[A] extends FrequencyBuffer[A] {

  //TODO: Override newBuilder and maybe need to make a CanBuildFrom?
  //TODO: http://daily-scala.blogspot.com/2010/04/creating-custom-traversable.html

  private[this] val frequencies: mutable.Map[A, AtomicLong] = mutable.Map.empty

  override def +=(pair: (A, Long)): FrequencyBuffer[A] = {
    getCounter(pair._1).addAndGet(pair._2)
    this
  }

  override def ++=(pairs: Traversable[(A, Long)]): FrequencyBuffer[A] = {
    pairs.foreach(pair => this += pair)
    this
  }

  override def +(pair: (A, Long)): Frequency[A] = toFrequency + pair

  override def ++(pairs: Traversable[(A, Long)]): Frequency[A] = toFrequency ++ pairs

  override def count: Long = frequencies.values.foldLeft(0L)(_.longValue + _.longValue)

  override def get(a: A): Option[Long] = frequencies.get(a).map(_.longValue)

  override def domain(implicit dops: DomainOps[A]): Seq[A] = frequencies.keys.toSeq.sorted(dops.ordering)

  override def foreach[U](f: ((A, Long)) => U): Unit = frequencies.foreach { case (a, n) => (a, n.longValue) }

  override def copy: FrequencyBuffer[A] = FrequencyBuffer(this)

  /**
    * Returns an immutable copy of this frequency buffer.
    */
  override def toFrequency: Frequency[A] = Frequency(this)

  override def toPDF: PDF[A] = ???

  /**
    * Get the counter, creating it if necessary, in a thread safe way.
    */
  private[this] def getCounter(a: A): AtomicLong = this.synchronized {
    frequencies.get(a) match {
      case Some(counter) =>
          counter
      case None =>
          val counter = new AtomicLong(0L)
          frequencies += a -> new AtomicLong(0L)
          counter
    }
  }

}

object AtomicFrequencyBuffer {

  def empty[A](): AtomicFrequencyBuffer[A] = new AtomicFrequencyBuffer[A]

  def apply[A](): AtomicFrequencyBuffer[A] = empty()

  def apply[A](pairs: Traversable[(A, Long)]): AtomicFrequencyBuffer[A] = {
    val buf = empty[A]()
    buf ++= pairs
    buf
  }

}
