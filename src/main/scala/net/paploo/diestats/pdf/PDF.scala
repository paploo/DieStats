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

  /**
   * Given a set of key/value pairs, aggregates them and normalizes them to
   * produce a [[PDF]].
   *
   * @example
   *          {{{PDF(1->1,1->2,3->1)}}}
   *          becomes
   *          {{{PDF(1->0.75, 3->0.25)}}}
   * @see [[fromSeq]]
   */
  def apply[A, B](pairs: (A, B)*)(implicit fk: A => Int, fv: B => Double): PDF = fromSeq(pairs)

  //Can't do this due to type erasure.
  //def apply[A,B](seq: Seq[(A,B)])(implicit fk: A => Int, fv: B => Double): PDF = fromSeq(seq)

  //Not actually useful.
  //def apply[A,B](map: Map[A,B])(implicit fk: A => Int, fv: B => Double): PDF = fromSeq(map.toSeq)

  /**
   * Converts the given sequence into a [[PDF]]. This sequence must be made of
   * (A,B) pairs that can be converted to (Int, Double) pairs.
   *
   * The given sequence undergoes three steps of processing before made into a PDF:
   *  - The sequence is converted into a sequence of Seq[(Int,Double)]
   *  - The pairs are reduced to unique keys that are the sums of all the values seen for that key.
   *  - It is normalized.
   * @see [[apply]]
   * @param inputSeq The input sequence of (A,B) pairs.
   * @param fk The implicit conversion function from A => Int
   * @param fv The implicit conversion function from B => Double
   * @tparam A The source seq key type
   * @tparam B The source seq value type
   * @return A [[PDF]]
   */
  def fromSeq[A, B](inputSeq: Seq[(A, B)])(implicit fk: A => Int, fv: B => Double): PDF = {
    val typedSeq = inputSeq.map {case (k, v) => (fk(k), fv(v))}
    val valueMap = typedSeq.groupBy(_._1).mapValues(seq => seq.map(_._2).sum)
    val sum = valueMap.foldLeft(0.0)((acc, pair) => acc + pair._2)
    val pairs = valueMap.mapValues(_ / sum).toSeq
    new PDF(SortedMap(pairs: _*))
  }

  // Doesn't work because of the implicit conversions that need to get passed through to fromSeq.
  //def newBuilder: mutable.Builder[(Int, Double), PDF] = new ArrayBuffer[(Int, Double)] mapResult fromSeq

  def newBuilder: mutable.Builder[(Int, Double), PDF] = new ArrayBuffer[(Int, Double)].mapResult(s => fromSeq(s))

  implicit def canBuildFrom: CanBuildFrom[PDF, (Int, Double), PDF] = new CanBuildFrom[PDF, (Int, Double), PDF] {
    def apply(): mutable.Builder[(Int, Double), PDF] = newBuilder

    def apply(from: PDF): mutable.Builder[(Int, Double), PDF] = newBuilder
  }
}

/**
 * Encapsulates a discrete probability density function whose domain is integers.
 *
 * The PDF is immutable and always normalized. This means much care must be
 * taken when using methods such as [[take]] because the result will be
 * normalized on you! You will often want to use [[toSeq]] to first get a non
 * normalizable sequence.
 *
 * @param map A SortedMap that acts as the backing store for this [[PDF]]
 */
final class PDF private(map: SortedMap[Int, Double]) extends Iterable[(Int, Double)] with IterableLike[(Int, Double), PDF] {

  lazy val minKey: Int = map.keys.min

  lazy val maxKey: Int = map.keys.max

  def apply(key: Int) = map.getOrElse(key, 0.0)

  override def iterator: Iterator[(Int, Double)] = keysIterator.map(k => (k, apply(k)))

  def keysIterator: Iterator[Int] = Range(minKey, maxKey + 1).iterator

  def keys: Iterable[Int] = keysIterator.toSeq

  def values: Iterable[Double] = keysIterator.toSeq.map(apply)

  /** Return the mean key value as a Double */
  def meanKey: Double = foldLeft(0.0)((acc, pair) => acc + (pair._1 * pair._2))

  /** Return the median key value as an Int.
    *
    * This will be the first key found where the CDF is at or above 50%
    */
  def medianKey: Int = {
    var sum = 0.0
    val result = find {
      case (k, v) =>
        sum = sum + v
        sum >= 0.5
    }
    result.get._1
  }

  /** Returns a set of the keys that had the highest value. */
  def modeKeys: SortedSet[Int] = modes.map(_._1)

  def modes: SortedSet[(Int, Double)] = {
    val groups = groupBy {case (k, v) => v}
    val maxGroupKey = groups.keys.max
    toSortedSet(groups(maxGroupKey).toSet)
  }

  /**
   * Composes this PDF with another one. This is done according to the following
   * rules:
   * For each combination of key-value pairs from the outer product, generate a new
   * pair whose key is the sum of the keys, and value is the product of the values.
   * These pairs are then combined and normalized into a new [[PDF]]
   *
   * For example, consider two die: When you compose their PDFs (each ranging
   * from 1 to 6), the new keys are their sums (2 to 12). For each combination
   * of keys, we compose the probabilities by multiplying them.  We then
   * collect up by key, adding the various probabilities found, and produce
   * a new [[PDF]]
   * @param other The other PDF
   * @return A new PDF
   */
  def compose(other: PDF): PDF = for {
    (k1, v1) <- this
    (k2, v2) <- other
    if v1 != 0.0 //Keep the product map sparse
    if v2 != 0.0 //Keep the product map sparse
    pair = (k1 + k2, v1 * v2) //This is because we re-normalize the inside maps before they are combined with flatMaps otherwise.
  } yield pair

  override def newBuilder: mutable.Builder[(Int, Double), PDF] = PDF.newBuilder

  override def toString(): String = s"${this.getClass.getSimpleName}(${map.toString()})"

  private def toSortedSet[T](set: Set[T])(implicit ord: Ordering[T]): SortedSet[T] = SortedSet(set.toSeq: _*)

}
