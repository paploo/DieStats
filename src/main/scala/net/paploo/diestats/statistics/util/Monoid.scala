package net.paploo.diestats.statistics.util

trait Monoid[A] {

  def concat(x: A, y: A): A

  def empty: A

}

object Monoid {

  class StringMonoid extends Monoid[String] {
    override def concat(x: String, y: String): String = x + y
    override val empty: String = ""
  }
  val stringMonoid: Monoid[String] = new StringMonoid()

  class AdditiveMonoid[N](implicit num: Numeric[N]) extends Monoid[N] {
    override def concat(x: N, y: N): N = num.plus(x, y)
    override def empty: N = num.zero
  }
  def additiveMonoid[N](implicit num: Numeric[N]): Monoid[N] = new AdditiveMonoid[N]()
  val additiveIntMonoid: Monoid[Int] = new AdditiveMonoid[Int]()
  val additiveLongMonoid: Monoid[Long] = new AdditiveMonoid[Long]()
  val additiveDoubleMonoid: Monoid[Double] = new AdditiveMonoid[Double]()
  val additiveProbabilityMonoid: Monoid[Probability] = new AdditiveMonoid[Probability]()(Probability.fractionalTypeclass)

  class MultiplicativeMonoid[N](implicit num: Numeric[N]) extends Monoid[N] {
    override def concat(x: N, y: N): N = num.times(x, y)
    override def empty: N = num.one
  }
  def multiplicativeMonoid[N](implicit num: Numeric[N]): Monoid[N] = new MultiplicativeMonoid[N]()
  val multiplicativeIntMonoid = new MultiplicativeMonoid[Int]()
  val multiplicativeLongMonoid = new MultiplicativeMonoid[Long]()
  val multiplicativeDoubleMonoid = new MultiplicativeMonoid[Double]()
  val multiplicativeProbabilityMonoid: Monoid[Probability] = new MultiplicativeMonoid[Probability]()(Probability.fractionalTypeclass)

  class SeqMonoid[A] extends Monoid[Seq[A]] {
    override def concat(x: Seq[A], y: Seq[A]): Seq[A] = x ++ y
    override def empty: Seq[A] = Vector.empty[A]
  }
  def seqMonoid[A]: Monoid[Seq[A]] = new SeqMonoid[A]()

  class SortedSeqMonoid[A](implicit ord: Ordering[A]) extends SeqMonoid[A] {
    override def concat(x: Seq[A], y: Seq[A]): Seq[A] = super.concat(x, y).sorted
  }
  def sortedSeqMonoid[A](implicit ord: Ordering[A]): Monoid[Seq[A]] = new SortedSeqMonoid[A]()

  trait Implicits {
    implicit val stringMonoid: Monoid[String] = Monoid.stringMonoid

    // Most of the time, domain operations are done on addition, so we default to addition.
    implicit val additiveIntMonoid: Monoid[Int] = Monoid.additiveIntMonoid
    implicit val additiveLongMonoid: Monoid[Long] = Monoid.additiveLongMonoid
    implicit val additiveDoubleMonoid: Monoid[Double] = Monoid.additiveDoubleMonoid

    // Probabilities are more frequently multiplied than added.
    implicit val additiveProbabilityMonoid: Monoid[Probability] = Monoid.multiplicativeProbabilityMonoid

    implicit def seqMonoid[A]: Monoid[Seq[A]] = Monoid.seqMonoid
  }

}
