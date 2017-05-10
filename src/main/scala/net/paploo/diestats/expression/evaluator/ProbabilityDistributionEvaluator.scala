package net.paploo.diestats.expression.evaluator

import net.paploo.diestats.statistics.frequency.FrequencyDistribution
import net.paploo.diestats.statistics.probabilitydistribution.ProbabilityDistribution
import net.paploo.diestats.statistics.util.Monoid

import scala.collection.immutable.NumericRange

trait ProbabilityDistributionEvaluator[A] extends Evaluator[A, ProbabilityDistribution[A]]
  with StringMemoryEvaluator.StringMemoryMapEvaluator[A, ProbabilityDistribution[A]]

object ProbabilityDistributionEvaluator {

  def ordered[A](implicit mon: Monoid[A], ord: Ordering[A]): Ordered[A] = new Ordered[A] {
    override def ordering: Ordering[A] = ord
    override def monoid: Monoid[A] = mon
  }

  def numeric[A](implicit num: Integral[A]): Numeric[A] = new Numeric[A] {
    override def numeric: Integral[A] = num
  }

  trait Monoidal[A] extends ProbabilityDistributionEvaluator[A] {
    def monoid: Monoid[A]

    override def fromValues(as: Iterable[A]): ProbabilityDistribution[A] =
      FrequencyDistribution.buildFromValues(as).toProbabilityDistribution

    override def convolve(x: ProbabilityDistribution[A], y: ProbabilityDistribution[A]): ProbabilityDistribution[A] =
      x.convolve(y)(monoid)
  }

  trait Ordered[A] extends Monoidal[A] with OrderedEvaluator[A, ProbabilityDistribution[A]] {
    def ordering: Ordering[A]

    override def best(n: Int, xs: Iterable[ProbabilityDistribution[A]]): ProbabilityDistribution[A] =
      ProbabilityDistribution.mapConvolve(xs)(_.sorted(ordering).takeRight(n).reduce(monoid.concat))

    override def worst(n: Int, xs: Iterable[ProbabilityDistribution[A]]): ProbabilityDistribution[A] =
      ProbabilityDistribution.mapConvolve(xs)(_.sorted(ordering).take(n).reduce(monoid.concat))
  }

  trait Numeric[A] extends Ordered[A] with NumericEvaluator[A, ProbabilityDistribution[A]] {
    def numeric: Integral[A]
    override def ordering: Ordering[A] = numeric
    override def monoid: Monoid[A] = Monoid.AdditiveMonoid(numeric)

    override def minus(x: ProbabilityDistribution[A], y: ProbabilityDistribution[A]): ProbabilityDistribution[A] =
      x.convolve(y.mapDomain(numeric.negate))(monoid)

    override def times(x: ProbabilityDistribution[A], y: ProbabilityDistribution[A]): ProbabilityDistribution[A] =
      x.convolve(y)(Monoid.MultiplicativeMonoid(numeric))

    override def quot(x: ProbabilityDistribution[A], y: ProbabilityDistribution[A]): ProbabilityDistribution[A] =
      x.convolve(y)(Monoid(numeric.one)(numeric.quot))
    //quot isn't a real monoid (it breaks the monoid laws), but it gives us the correct behavior for convolve.
    //Normally you'd want to invert the elements on y, and then use the monoid, but for fractional there isn't a good way to do this with multiplication, so we cheat.

    override def negate(x: ProbabilityDistribution[A]): ProbabilityDistribution[A] =
      x.mapDomain(numeric.negate)

    override def rangedValues(max: A): ProbabilityDistribution[A] = rangedValues(numeric.one, max)

    override def rangedValues(min: A, max: A): ProbabilityDistribution[A] = fromValues(
      NumericRange.inclusive(min, max, numeric.one)(numeric)
    )
  }

}