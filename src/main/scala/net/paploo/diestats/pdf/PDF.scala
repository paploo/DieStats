package net.paploo.diestats.pdf

import scala.collection.immutable.SortedMap
import scala.collection.IterableLike
import scala.collection.mutable.Builder
import scala.collection.generic.CanBuildFrom
import scala.collection.mutable.ArrayBuffer

object PDF {
  lazy val empty: PDF = new PDF(SortedMap.empty)

  def apply[A,B](pairs: (A,B)*)(implicit fk: A => Int, fv: B => Double): PDF = fromMap( Map(pairs: _*) )

  def apply[A,B](inputMap: scala.collection.Map[A,B])(implicit fk: A => Int, fv: B => Double): PDF = fromMap(inputMap)(fk,fv)

  def fromMap[A, B](inputMap: scala.collection.Map[A, B])(implicit fk: A => Int, fv: B => Double): PDF = {
    val m = inputMap.map {
      case (k, v) => (fk(k) -> fv(v))
    }
    val sum = m.foldLeft(0.0)((acc, pair) => acc + pair._2)
    val s = m.toSeq.map {
      case (k, v) => (k -> (v / sum))
    }
    new PDF(SortedMap(s: _*))
  }

  def newBuilder: Builder[(Int,Double), PDF] = new ArrayBuffer[(Int,Double)] mapResult (x => PDF(x.toSeq: _*))

  implicit def canBuildFrom: CanBuildFrom[PDF, (Int, Double), PDF] = new CanBuildFrom[PDF, (Int, Double), PDF] {
    def apply(): Builder[(Int, Double), PDF] = newBuilder
    def apply(from: PDF): Builder[(Int, Double), PDF] = newBuilder
  }
}

final class PDF private(map: SortedMap[Int, Double]) extends Iterable[(Int, Double)] with IterableLike[(Int,Double), PDF] {

  private lazy val minKey: Int = map.keys.min

  private lazy val maxKey: Int = map.keys.max

  def apply(key: Int) = map.getOrElse(key, 0.0)

  override def iterator: Iterator[(Int, Double)] = Range(minKey, maxKey).iterator.map(k => (k, apply(k)))

  override def newBuilder: Builder[(Int,Double), PDF] = PDF.newBuilder

  def keys: Iterable[Int] = Range(minKey, maxKey).toSeq

  def values: Iterable[Double] = Range(minKey, maxKey).toSeq.map(apply(_))

  override def toString: String = s"${this.getClass.getSimpleName}(${map.toString})"

}
