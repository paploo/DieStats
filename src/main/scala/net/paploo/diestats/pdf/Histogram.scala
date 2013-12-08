package net.paploo.diestats.pdf

import scala.collection.mutable

class Histogram[T] extends mutable.HashMap[T, Long] {

  def sum = foldLeft[Long](0)((acc, pair) => acc + pair._2)

  override def default(key: T) = getOrElse(key, 0)

  def <<(key: T): this.type = {
    val newValue: Long = this(key) + 1
    this += (key -> newValue)
  }

  def compose(other: Histogram[T]): Histogram[T] = {
    for ((k, v) <- other) yield this(k) = this(k) + v
    this
  }

  //def toPDF(implicit fk: T => Int, fv: Long => Double) = PDF.fromMap[T, Long](this)(fk, fv)
}
