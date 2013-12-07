package net.paploo.diestats.pdf

import scala.collection.immutable.SortedMap
import scala.collection.IterableLike
import scala.collection.mutable
import scala.collection.generic.CanBuildFrom
import scala.collection.mutable.ArrayBuffer

object PDF {
  lazy val empty: PDF = new PDF(SortedMap.empty)

  def apply[A, B](pairs: (A, B)*)(implicit fk: A => Int, fv: B => Double): PDF = fromMap(Map(pairs: _*))

  def apply[A, B](inputMap: scala.collection.Map[A, B])(implicit fk: A => Int, fv: B => Double): PDF = fromMap(inputMap)(fk, fv)

  def fromMap[A, B](inputMap: scala.collection.Map[A, B])(implicit fk: A => Int, fv: B => Double): PDF = {
    val m = inputMap.map {
      case (k, v) => fk(k) -> fv(v)
    }
    val sum = m.foldLeft(0.0)((acc, pair) => acc + pair._2)
    val s = m.toSeq.map {
      case (k, v) => k -> (v / sum)
    }
    new PDF(SortedMap(s: _*))
  }

  def newBuilder: mutable.Builder[(Int, Double), PDF] = new ArrayBuffer[(Int, Double)] mapResult (x => PDF(x.toSeq: _*))

  implicit def canBuildFrom: CanBuildFrom[PDF, (Int, Double), PDF] = new CanBuildFrom[PDF, (Int, Double), PDF] {
    def apply(): mutable.Builder[(Int, Double), PDF] = newBuilder

    def apply(from: PDF): mutable.Builder[(Int, Double), PDF] = newBuilder
  }
}

final class PDF private(map: SortedMap[Int, Double]) extends Iterable[(Int, Double)] with IterableLike[(Int, Double), PDF] {

  private lazy val minKey: Int = map.keys.min

  private lazy val maxKey: Int = map.keys.max

  def apply(key: Int) = map.getOrElse(key, 0.0)

  override def iterator: Iterator[(Int, Double)] = keysIterator.map(k => (k, apply(k)))

  def keysIterator: Iterator[Int] = Range(minKey, maxKey+1).iterator

  def keys: Iterable[Int] = keysIterator.toSeq

  def values: Iterable[Double] = keysIterator.toSeq.map(apply)

  def compose(other: PDF) = {
    val tuples = for {
      (k1, v1) <- this.toList
      (k2, v2) <- other.toList
    } yield (k1+k2, v1*v2)

    val m = tuples.groupBy(_._1).mapValues(_.map(_._2).sum)

    PDF(m)
  }

  override def newBuilder: mutable.Builder[(Int, Double), PDF] = PDF.newBuilder

  override def toString(): String = s"${this.getClass.getSimpleName}(${map.toString()})"

}
