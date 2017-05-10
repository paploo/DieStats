package net.paploo.diestats.statistics.util

@annotation.implicitNotFound(msg = "No implicit Monoid defined for ${A}.")
trait Monoid[A] {
  def concat(x: A, y: A): A
  def empty: A

  def reduce(as: Iterable[A]): A = as.foldLeft(empty)((x,y) => concat(x,y))
}

object Monoid {

  def apply[A](emptyValue: => A)(concatFunction: (A, A) => A): Monoid[A] = new Monoid[A] {
    override def concat(x: A, y: A): A = concatFunction(x, y)
    override def empty: A = emptyValue
  }

  class StringMonoid extends Monoid[String] {
    override def concat(x: String, y: String): String = x + y
    override val empty: String = ""
  }
  implicit object StringMonoid extends StringMonoid

  class AdditiveMonoid[N](implicit num: Numeric[N]) extends Monoid[N] {
    override def concat(x: N, y: N): N = num.plus(x, y)
    override def empty: N = num.zero
  }
  def AdditiveMonoid[N](implicit num: Numeric[N]): Monoid[N] = new AdditiveMonoid[N]()
  implicit val AdditiveIntMonoid: Monoid[Int] = new AdditiveMonoid[Int]()
  implicit val AdditiveLongMonoid: Monoid[Long] = new AdditiveMonoid[Long]()
  implicit val AdditiveDoubleMonoid: Monoid[Double] = new AdditiveMonoid[Double]()
  implicit val AdditiveProbabilityMonoid: Monoid[Probability] = new AdditiveMonoid[Probability]()(Probability.ProbabilityIsConflicted)

  class MultiplicativeMonoid[N](implicit num: Numeric[N]) extends Monoid[N] {
    override def concat(x: N, y: N): N = num.times(x, y)
    override def empty: N = num.one
  }
  def MultiplicativeMonoid[N](implicit num: Numeric[N]): Monoid[N] = new MultiplicativeMonoid[N]()
  val MultiplicativeIntMonoid = new MultiplicativeMonoid[Int]()
  val MultiplicativeLongMonoid = new MultiplicativeMonoid[Long]()
  val MultiplicativeDoubleMonoid = new MultiplicativeMonoid[Double]()
  val MultiplicativeProbabilityMonoid: Monoid[Probability] = new MultiplicativeMonoid[Probability]()(Probability.ProbabilityIsConflicted)

  class SeqMonoid[A] extends Monoid[Seq[A]] {
    override def concat(x: Seq[A], y: Seq[A]): Seq[A] = x ++ y
    override def empty: Seq[A] = Vector.empty[A]
  }
  implicit def SeqMonoid[A]: Monoid[Seq[A]] = new SeqMonoid[A]()

  class SortedSeqMonoid[A](implicit ord: Ordering[A]) extends SeqMonoid[A] {
    override def concat(x: Seq[A], y: Seq[A]): Seq[A] = super.concat(x, y).sorted
  }
  def SortedSeqMonoid[A](implicit ord: Ordering[A]): Monoid[Seq[A]] = new SortedSeqMonoid[A]()

}
