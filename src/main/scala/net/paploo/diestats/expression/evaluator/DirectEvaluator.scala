package net.paploo.diestats.expression.evaluator

import net.paploo.diestats.statistics.util.Monoid

import scala.collection.immutable.NumericRange

/**
  * Implementation of an evaluator that directly evaluates like a classic expression.
  * @param random
  * @param numeric
  * @tparam A The domain type
  */
class DirectEvaluator[A](override val random: java.util.Random)(implicit override val numeric: Integral[A])
  extends DirectEvaluator.Numeric[A]
    with DirectEvaluator.RandomValue[A]
    with StringMemoryEvaluator.StringMemoryMapEvaluator[A, A]

object DirectEvaluator {

  def apply[A](implicit numeric: Integral[A]): DirectEvaluator[A] =
    apply(new java.util.Random())

  def apply[A](random: java.util.Random)(implicit numeric: Integral[A]): DirectEvaluator[A] =
    new DirectEvaluator[A](random)


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

}
