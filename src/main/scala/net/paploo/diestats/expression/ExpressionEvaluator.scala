package net.paploo.diestats.expression

import net.paploo.diestats.statistics.frequency.FrequencyDistribution
import net.paploo.diestats.statistics.probabilitydistribution.ProbabilityDistribution
import net.paploo.diestats.statistics.util.RandomGenerator

/**
  * Trait expressing an implementation of operations over expressions on domain A,
  * returning value R.
  *
  * Example concrete types for R include a ProbabilityDistribution for analysis, or even A in random value generation.
  *
  * ExpressionOps contains a mixture of operations that are expected, but only a
  * subset can be done on some domains. In the current design, it is up to the
  * implementation do decide how to handle non-sensical operations for a given
  * domain. For example, some consumers of this library may decide to throw
  * exceptions, while others may parameterize R over an Either or Try, while
  * others may make specialized validation ExpressionOps implementations for
  * a domain.
  *
  * @tparam A The domain
  * @tparam R The operation aggregation value.
  */
trait ExpressionEvaluator[A, +R] {

  def fromValues(values: Iterable[A]): R

  def fromCounts(valueCounts: Iterable[(A, Long)]): R

  def plus(x: Expression[A], y: Expression[A]): R

  def minus(x: Expression[A], y: Expression[A]): R

  def times(x: Expression[A], y: Expression[A]): R

  def div(x: Expression[A], y: Expression[A]): R

  def negate(x: Expression[A]): R

  def repeat(times: Int, x: Expression[A]): R

  def takeLower(n: Int, exprs: Iterable[Expression[A]]): R

  def takeUpper(n: Int, exprs: Iterable[Expression[A]]): R

}

object ExpressionEvaluator {

  trait RNGEvaluator[A] extends ExpressionEvaluator[A, A]

  trait ProbabilityDistributionEvaluator[A] extends ExpressionEvaluator[A, ProbabilityDistribution[A]]

  /**
    * The most common domain—used in almost all board games—is an Int domain.
    *
    * We define both a ProbabilityDistribution implementation, and a random value generation implementation.
    */
  object IntEvaluators {

    class RNGEvaluator(implicit rand: RandomGenerator) extends ExpressionEvaluator[Int, Int] {

      override def fromValues(values: Iterable[Int]): Int =
        FrequencyDistribution.buildFromValues(values).toStatistics.selectRandom

      override def fromCounts(valueCounts: Iterable[(Int, Long)]): Int =
        FrequencyDistribution.buildFrom(valueCounts).toStatistics.selectRandom

      override def plus(x: Expression[Int], y: Expression[Int]): Int = x(this) + y(this)

      override def minus(x: Expression[Int], y: Expression[Int]): Int = x(this) - y(this)

      override def times(x: Expression[Int], y: Expression[Int]): Int = x(this) * y(this)

      override def div(x: Expression[Int], y: Expression[Int]): Int = x(this) / y(this)

      override def negate(x: Expression[Int]): Int = -x(this)

      override def repeat(times: Int, x: Expression[Int]): Int =
        (0 until times).reduce((s,_) => s + x(this))

      override def takeLower(n: Int, exprs: Iterable[Expression[Int]]): Int =
        exprs.map(_.apply(this)).toSeq.sorted.take(n).sum

      override def takeUpper(n: Int, exprs: Iterable[Expression[Int]]): Int =
        exprs.map(_.apply(this)).toSeq.sorted.reverse.take(n).sum

    }

    class PrbabilityDistribution extends ExpressionEvaluator[Int, ProbabilityDistribution[Int]] {

      override def fromValues(values: Iterable[Int]): ProbabilityDistribution[Int] = ???

      override def fromCounts(valueCounts: Iterable[(Int, Long)]): ProbabilityDistribution[Int] = ???

      override def plus(x: Expression[Int], y: Expression[Int]): ProbabilityDistribution[Int] = ???

      override def minus(x: Expression[Int], y: Expression[Int]): ProbabilityDistribution[Int] = ???

      override def times(x: Expression[Int], y: Expression[Int]): ProbabilityDistribution[Int] = ???

      override def div(x: Expression[Int], y: Expression[Int]): ProbabilityDistribution[Int] = ???

      override def negate(x: Expression[Int]): ProbabilityDistribution[Int] = ???

      override def repeat(times: Int, x: Expression[Int]): ProbabilityDistribution[Int] = ???

      override def takeLower(n: Int, exprs: Iterable[Expression[Int]]): ProbabilityDistribution[Int] = ???

      override def takeUpper(n: Int, exprs: Iterable[Expression[Int]]): ProbabilityDistribution[Int] = ???

    }

  }

}