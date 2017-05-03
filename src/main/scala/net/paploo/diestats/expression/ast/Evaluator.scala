package net.paploo.diestats.expression.ast

import java.util.Random

import net.paploo.diestats.statistics.util.Monoid

import scala.annotation.tailrec

/**
  * The base trait for evaluators of expressions.
  *
  * Defines functionality that is expected of every domain.
  *
  * @tparam A The domain type
  * @tparam R The evaluation result type
  */
trait Evaluator[A, R] {
  def fromValues(as: Iterable[A]): R

  def convolve(x: R, y: R): R
  def repeatedConvolve(n: Int, x: R): R
}

/**
  * The base trait for evaluators over an ordered domain.
  *
  * Defines functionality that is expected of ordered domains.
  *
  * Best/Worst are the trickiest to think about implementing, but
  * the two main use cases are:
  * 1. When R is a ProbabilityDistribution[A], where mapConvolve is used, and
  * 2. When R =:= A, for example, when evaluating with a random generator.
  *
  * Note that only some sub implementations need define an ordering,
  * and hence none is required here by the trait. For example,
  * a stringifier won't need one, while something that performs the actual
  * computation will need it.
  *
  * @tparam A The domain type
  */
trait OrderedEvaluator[A, R] extends Evaluator[A, R] {
  def best(n: Int, xs: Iterable[R]): R
  def worst(n: Int, xs: Iterable[R]): R
}

/**
  * The base trait for evaluators over a numeric domain.
  *
  * Defines functionality that is expected of a numeric domain.
  *
  * Note that plus is almost always a synonym for convolution.
  *
  * @tparam A The domain type
  * @tparam R The evaluation result type
  */
trait NumericEvaluator[A, R] extends OrderedEvaluator[A, R] {
  def plus(x: R, y: R): R = convolve(x, y)
  def minus(x: R, y: R): R
  def times(x: R, y: R): R
  def div(x: R, y: R): R

  def negate(x: R)

  def range(max: A): R
  def range(min: A, max: A): R
}

object Evaluator {

  /**
    * A default stringifier that can be used with any expression.
    * @tparam A The domain type
    */
  trait Stringifier[A] extends NumericEvaluator[A, String] {
    override def convolve(x: String, y: String): String = s"($x + $y)"
    override def repeatedConvolve(n: Int, x: String): String = s"($n $x)"
    override def minus(x: String, y: String): String = s"($x - $y)"
    override def times(x: String, y: String): String = s"($x x $y)"
    override def div(x: String, y: String): String = s"($x / $y)"
    override def negate(x: String): Unit = s"(-$x)"
    override def best(n: Int, xs: Iterable[String]): String = s"($xs b $n)"
    override def worst(n: Int, xs: Iterable[String]): String = s"($xs w $n)"

    override def fromValues(as: Iterable[A]): String = setString(as)
    override def range(max: A): String = s"d$max"
    override def range(min: A, max: A): String = s"d{$min-$max}"

    private[this] def setString(as: Iterable[A]): String = as.mkString("{", ", ", "}")
  }
  def Stringifier[A]: Stringifier[A] = new Stringifier[A] {}


  trait DefaultEvaluator[A, R] extends Evaluator[A, R] {

    override def repeatedConvolve(n: Int, x: R): R = {
      require(n > 0, s"repeatedConvolution must hapeen more than zero times, but got $n, when convolving over $x")
      Seq.fill(n)(x).reduce(convolve)
    }

  }

  trait ReflexiveMonoidal[A, R] extends DefaultEvaluator[A, R] {
    def monoid: Monoid[R]

    override def convolve(x: R, y: R): R = monoid.concat(x, y)
  }

  trait ReflexiveOrdered[A] extends ReflexiveMonoidal[A, A] with OrderedEvaluator[A, A] {
    def ordering: Ordering[A]

    override def best(n: Int, xs: Iterable[A]): A =
      monoid.reduce(xs.toSeq.sorted(ordering).takeRight(n))

    override def worst(n: Int, xs: Iterable[A]): A =
      monoid.reduce(xs.toSeq.sorted(ordering).take(n))
  }

  trait ReflexiveNumeric[A] extends ReflexiveOrdered[A] with NumericEvaluator[A, A] {
    def numeric: Numeric[A]
    override def ordering: Ordering[A] = numeric
    override def monoid: Monoid[A] = Monoid.AdditiveMonoid(numeric)

    override def minus(x: A, y: A): A = numeric.minus(x, y)
    override def times(x: A, y: A): A = numeric.times(x, y)
    override def div(x: A, y: A): A = numeric.fromInt(numeric.toInt(x) / numeric.toInt(y))

    override def negate(x: A): Unit = numeric.negate(x)

    override def range(max: A): A = range(numeric.one, max)
    override def range(min: A, max: A): A = fromValues(computeRange(Vector.empty[A], min, max))

    @tailrec
    private[this] def computeRange(as: Seq[A], current: A, stopAt: A): Seq[A] = current match {
      case curr if numeric.gt(curr, stopAt) => as
      case curr =>
        val next = numeric.plus(curr, numeric.one)
        computeRange(as :+ next, next, stopAt)
    }

  }

  trait RandomReflexive[A] extends DefaultEvaluator[A, A] {
    def random: java.util.Random

    override def fromValues(as: Iterable[A]): A = as.toSeq(random.nextInt(as.size))
  }

  class RandomNumericReflexive[A](implicit override val numeric: Numeric[A]) extends ReflexiveNumeric[A] with RandomReflexive[A] {
    override def random: Random = new java.util.Random()
  }
  def RandomNumericReflexive[A](implicit numeric: Numeric[A]) = new RandomNumericReflexive()

}