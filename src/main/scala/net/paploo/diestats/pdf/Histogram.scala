package net.paploo.diestats.pdf

import scala.collection.mutable

/**
 * A mutable accumulation buffer for histogram counts that can easily be converted to a [[PDF]] instance.
 *
 * The primary methods of interest are [[<<]], [[compose]], and [[toPDF]].
 *@tparam T Any type that can be implicitly converted to an Integer later.
 */
@deprecated("Replaced by statistics.mutable.Frequency", "0.2.0")
class Histogram[T] extends mutable.HashMap[T, Long] {

  override def default(key: T) = getOrElse(key, 0)

  /**
   * Adds a single count for the passed key.
   * @param key The key to increment the count for.
   * @return This.
   */
  def <<(key: T): this.type = this << (key -> 1)

  /**
   * Adds the given count to the passed key.
   * @param pair A tuple of (key, count)
   * @tparam U A type that can be implicitly cast to Long.
   * @return This.
   */
  def <<[U <% Long](pair: (T,U)): this.type = {
      val newValue: Long = this(pair._1) + pair._2
      this += (pair._1 -> newValue)
  }

  /**
   * Destructively assimilates the counts from other into this histogram.
   * @param other Other histogram.
   * @return This.
   */
  def compose(other: Histogram[T]): Histogram[T] = {
    for ((k, v) <- other) yield this(k) = this(k) + v
    this
  }

  /**
   * Converts the histogram to an immutable PDF
   * @param fk Implicit conversion function from T => Int
   * @param fv Implicit conversion function from Long => Double
   * @return A PDF.
   */
  def toPDF(implicit fk: T => Int, fv: Long => Double) = PDF.fromSeq(toSeq)
}
