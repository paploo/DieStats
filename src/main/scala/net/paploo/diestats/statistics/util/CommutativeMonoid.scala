package net.paploo.diestats.statistics.util

/**
  * Marker trait for Monoids that obey commutativity:
  * concat(x, y) === concat(y, x)
  * @tparam A
  */
trait CommutativeMonoid[A] extends Monoid[A]

object CommutativeMonoid {

  def apply[A](emptyValue: => A)(concatFunction: (A, A) => A): CommutativeMonoid[A] = new CommutativeMonoid[A] {
    override def concat(x: A, y: A): A = concatFunction(x, y)
    override def empty: A = emptyValue
  }

  class AdditiveMonoid[N](implicit num: Numeric[N]) extends CommutativeMonoid[N] {
    override def concat(x: N, y: N): N = num.plus(x, y)
    override def empty: N = num.zero
  }
  def AdditiveMonoid[N](implicit num: Numeric[N]): CommutativeMonoid[N] = new AdditiveMonoid[N]()
  implicit val AdditiveIntMonoid: CommutativeMonoid[Int] = new AdditiveMonoid[Int]()
  implicit val AdditiveLongMonoid: CommutativeMonoid[Long] = new AdditiveMonoid[Long]()
  implicit val AdditiveDoubleMonoid: CommutativeMonoid[Double] = new AdditiveMonoid[Double]()
  implicit val AdditiveProbabilityMonoid: CommutativeMonoid[Probability] = new AdditiveMonoid[Probability]()(Probability.ProbabilityIsConflicted)

  class MultiplicativeMonoid[N](implicit num: Numeric[N]) extends CommutativeMonoid[N] {
    override def concat(x: N, y: N): N = num.times(x, y)
    override def empty: N = num.one
  }
  def MultiplicativeMonoid[N](implicit num: Numeric[N]): CommutativeMonoid[N] = new MultiplicativeMonoid[N]()
  val MultiplicativeIntMonoid: CommutativeMonoid[Int] = new MultiplicativeMonoid[Int]()
  val MultiplicativeLongMonoid: CommutativeMonoid[Long] = new MultiplicativeMonoid[Long]()
  val MultiplicativeDoubleMonoid: CommutativeMonoid[Double] = new MultiplicativeMonoid[Double]()
  val MultiplicativeProbabilityMonoid: Monoid[Probability] = new MultiplicativeMonoid[Probability]()(Probability.ProbabilityIsConflicted)

  /**
    * Often used with convolve where the order doesn't matter, but the counts do;
    * in most cases, it is clearer to convolve over a Map[A, Long] instead.
    *
    * Example:
    * convolving a coin, Seq(Tails) and Seq(Heads), with itself will combine the two results
    * of a heads and a tails into a single value, Seq(Tails, Heads), instead of one for
    * Seq(Tails, Heads) and another for Seq(Heads, Tails).
    * @param ord
    * @tparam A
    */
  class SeqMonoid[A](implicit ord: Ordering[A]) extends CommutativeMonoid[Seq[A]] {
    override def concat(x: Seq[A], y: Seq[A]): Seq[A] = (x ++ y).sorted
    override def empty: Seq[A] = Vector.empty[A]
  }
  def SeqMonoid[A](implicit ord: Ordering[A]): CommutativeMonoid[Seq[A]] = new SeqMonoid[A]()

}