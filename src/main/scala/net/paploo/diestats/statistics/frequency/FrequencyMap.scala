package net.paploo.diestats.statistics.frequency
import net.paploo.diestats.statistics.pdf.PDF

class FrequencyMap[A](frequencies: Map[A, Long]) extends Frequency[A] {

  override def +(pair: (A, Long)): Frequency[A] = FrequencyMap(frequencies + pair)

  override def ++(pairs: Traversable[(A, Long)]): Frequency[A] = FrequencyMap(frequencies ++ pairs)

  override def get(a: A): Option[Long] = frequencies.get(a)

  lazy val count: Long = frequencies.values.sum

  override def toFrequency: Frequency[A] = this

  override def toPDF: PDF[A] = ???

  override def foreach[U](f: ((A, Long)) => U): Unit = frequencies.foreach(f)
}

object FrequencyMap {

  def empty[A]: FrequencyMap[A] = new FrequencyMap(Map.empty)

  def apply[A](): FrequencyMap[A] = empty

  def apply[A](pairs: Traversable[(A, Long)]): FrequencyMap[A] = new FrequencyMap(pairs.toMap)

}