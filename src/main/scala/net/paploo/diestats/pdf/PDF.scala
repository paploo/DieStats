package net.paploo.diestats.pdf

import scala.collection.immutable.SortedMap
import scala.collection.immutable.SortedSet
import scala.collection.IterableLike
import scala.collection.mutable
import scala.collection.generic.CanBuildFrom
import scala.collection.mutable.ArrayBuffer
import scala.language.implicitConversions

object PDFSeq {
  implicit def apply[A, B](seq: Seq[(A, B)]): PDFSeq[A, B] = new PDFSeq(seq)

  implicit def unapply[A, B](pseq: PDFSeq[A, B]): Option[Seq[(A, B)]] = Some(pseq.toSeq)

  implicit def toSeq[A, B](pseq: PDFSeq[A, B]): Seq[(A, B)] = pseq.toSeq
}

class PDFSeq[A, B](seq: Seq[(A, B)]) {
  def toPDF(implicit fk: A => Int, fv: B => Double) = PDF.fromSeq(seq)

  def toSeq = seq
}

object PDF {
  lazy val empty: PDF = new PDF(SortedMap.empty)

  def apply[A, B](pairs: (A, B)*)(implicit fk: A => Int, fv: B => Double): PDF = fromSeq(pairs)

  //Can't do this due to type erasure.
  //def apply[A,B](seq: Seq[(A,B)])(implicit fk: A => Int, fv: B => Double): PDF = fromSeq(seq)

  //Not actually useful.
  //def apply[A,B](map: Map[A,B])(implicit fk: A => Int, fv: B => Double): PDF = fromSeq(map.toSeq)

  def fromSeq[A, B](inputSeq: Seq[(A, B)])(implicit fk: A => Int, fv: B => Double): PDF = {
    val typedSeq = inputSeq.map {
      case (k, v) => (fk(k), fv(v))
    }
    val valueMap = typedSeq.groupBy(_._1).mapValues(seq => seq.map(_._2).sum)
    val sum = valueMap.foldLeft(0.0)((acc, pair) => acc + pair._2)
    val pairs = valueMap.mapValues(_ / sum).toSeq
    new PDF(SortedMap(pairs: _*))
  }

  // Doesn't work because of the implicit converstions that need to get passed through to fromSeq.
  //def newBuilder: mutable.Builder[(Int, Double), PDF] = new ArrayBuffer[(Int, Double)] mapResult fromSeq

  def newBuilder: mutable.Builder[(Int, Double), PDF] = (new ArrayBuffer[(Int, Double)]).mapResult(s => fromSeq(s))

  implicit def canBuildFrom: CanBuildFrom[PDF, (Int, Double), PDF] = new CanBuildFrom[PDF, (Int, Double), PDF] {
    def apply(): mutable.Builder[(Int, Double), PDF] = newBuilder

    def apply(from: PDF): mutable.Builder[(Int, Double), PDF] = newBuilder
  }
}

final class PDF private(map: SortedMap[Int, Double]) extends Iterable[(Int, Double)] with IterableLike[(Int, Double), PDF] {

  lazy val minKey: Int = map.keys.min

  lazy val maxKey: Int = map.keys.max

  def apply(key: Int) = map.getOrElse(key, 0.0)

  override def iterator: Iterator[(Int, Double)] = keysIterator.map(k => (k, apply(k)))

  def keysIterator: Iterator[Int] = Range(minKey, maxKey + 1).iterator

  def keys: Iterable[Int] = keysIterator.toSeq

  def values: Iterable[Double] = keysIterator.toSeq.map(apply)

  /** Return the mean key value as a Double */
  def meanKey: Double = foldLeft(0.0)( (acc, pair) => acc + (pair._1 * pair._2))

  /** Return the median key value as an Int.
    *
    * This will be the first key found where the CDF is at or above 50%
    */
  def medianKey: Int = {
    var sum = 0.0
    val result = find {
      case (k,v) =>
        sum = sum + v
        sum >= 0.5
    }
    result.get._1
  }

  /** Returns a set of the keys that had the highest value. */
  def modeKeys: SortedSet[Int] = modes.map(_._1)

  def modes: SortedSet[(Int, Double)] = {
    val groups = groupBy {case (k,v) => v}
    val maxGroupKey = groups.keys.max
    toSortedSet( groups(maxGroupKey).toSet )
  }

  // TODO: Skip if either value is zero.
  /*
  * Note that without the assignment to pair, this compiles to:
  * pdf1.flatMap { (k1,v1) => pdf2.map { case (k2,v2) => (k1,k2, v1*v2) }
  * Which means the intermediate maps get renormalized before being combined in the flatmap phase.
  *
   */
  def compose(other: PDF): PDF = for {
    (k1, v1) <- this
    (k2, v2) <- other
    pair = (k1 + k2, v1 * v2) //This is because the renormalization causes the inter
  } yield pair

  override def newBuilder: mutable.Builder[(Int, Double), PDF] = PDF.newBuilder

  override def toString(): String = s"${this.getClass.getSimpleName}(${map.toString()})"

  private def toSortedSet[T](set: Set[T])(implicit ord: Ordering[T]): SortedSet[T] = SortedSet( set.toSeq: _* )

}
