package net.paploo.diestats.expression.evaluator

import net.paploo.diestats.statistics.util.Monoid

import scala.collection.immutable.NumericRange

/**
  * Base trait of evaluators that directly evaluates like a classic expression.
  * @tparam A The domain type
  */
trait DirectEvaluator[A] extends DirectEvaluator.RandomValue[A] with StringMemoryEvaluator.StringMemoryMapEvaluator[A, A]

object DirectEvaluator {

  def ordered[A](implicit monoid: Monoid[A], ordering: Ordering[A]): DirectOrderedEvaluator[A] =
    ordered(new java.util.Random())

  def ordered[A](random: java.util.Random)(implicit monoid: Monoid[A], ordering: Ordering[A]): DirectOrderedEvaluator[A] =
    new DirectOrderedEvaluator[A](random)

  def numeric[A](implicit num: Integral[A]): DirectNumericEvaluator[A] =
    numeric(new java.util.Random())

  def numeric[A](random: java.util.Random)(implicit num: Integral[A]): DirectNumericEvaluator[A] =
    new DirectNumericEvaluator[A](random)

  trait Monoidal[A] extends MonoidalEvaluator[A, A]

  trait RandomValue[A] extends Evaluator[A, A] {
    def random: java.util.Random

    override def fromValues(as: Iterable[A]): A = as.toSeq(random.nextInt(as.size))
  }

  trait Ordered[A] extends Monoidal[A] with OrderedEvaluator[A, A] {
    def ordering: Ordering[A]

    override def best(n: Int, xs: Iterable[A]): A =
      monoid.reduce(xs.toSeq.sorted(ordering).takeRight(n))

    override def worst(n: Int, xs: Iterable[A]): A =
      monoid.reduce(xs.toSeq.sorted(ordering).take(n))
  }

  trait Numeric[A] extends Ordered[A] with NumericEvaluator[A, A] {
    def numeric: Integral[A]
    override def ordering: Ordering[A] = numeric
    override def monoid: Monoid[A] = Monoid.AdditiveMonoid(numeric)

    override def minus(x: A, y: A): A = numeric.minus(x, y)
    override def times(x: A, y: A): A = numeric.times(x, y)
    override def quot(x: A, y: A): A = numeric.quot(x, y)

    override def negate(x: A): A = numeric.negate(x)

    override def rangedValues(max: A): A = rangedValues(numeric.one, max)
    override def rangedValues(min: A, max: A): A = fromValues(
      NumericRange.inclusive(min, max, numeric.one)(numeric)
    )

  }

  class DirectOrderedEvaluator[A](override val random: java.util.Random)(implicit override val monoid: Monoid[A], override val ordering: Ordering[A])
    extends DirectEvaluator[A] with DirectEvaluator.Ordered[A]

  class DirectNumericEvaluator[A](override val random: java.util.Random)(implicit override val numeric: Integral[A])
    extends DirectEvaluator[A] with DirectEvaluator.Numeric[A]

}
