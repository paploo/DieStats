package net.paploo.diestats.expression.ast

import java.util.Random

import net.paploo.diestats.statistics.util.Monoid

import scala.collection.immutable.NumericRange

/**
  * The base trait for evaluators of expressions.
  *
  * Defines functionality that is expected of every domain.
  *
  * @tparam A The domain type
  */
trait Evaluator[A] {
  type R

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
trait OrderedEvaluator[A] extends Evaluator[A] {
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
  */
trait NumericEvaluator[A] extends OrderedEvaluator[A] {
  def plus(x: R, y: R): R = convolve(x, y)
  def minus(x: R, y: R): R
  def times(x: R, y: R): R
  def quot(x: R, y: R): R

  def negate(x: R): R

  def rangedValues(max: A): R
  def rangedValues(min: A, max: A): R
}

/**
  * Trait used to mark an evaluator as having side effects within
  * the scope of the evaluator.
  *
  * These should be created via a factory and re-used only within a given
  * context (e.g. a set of expressions that share some state).
  *
  * ContextEvaluators should keep the side-effects bound
  * within this context, so that a new context can be created from
  * the factory and used independently.
  *
  * @tparam A The domain type
  */
trait ContextualEvaluator[A] extends Evaluator[A]

/**
  * Trait for an Evaluator that retains the contextual information of
  * a memory.
  *
  * @tparam A The domain type
  * @tparam I The ID type
  */
trait MemoryContext[A, I] extends ContextualEvaluator[A] {
  /**
    * Storage is typically implemented as a void return type, however
    * in an expression oriented anguage, it usually returns the values stored.
    * @param id The identifier for storage.
    * @param value The value to store.
    * @return The stored value.
    */
  def store(id: I, value: R): R
  def fetch(id: I): R
}

object Evaluator {

  /**
    * A default stringifier that can be used with any expression.
    * @tparam A The domain type
    */
  trait Stringifier[A] extends NumericEvaluator[A] {

    override type R = String

    override def convolve(x: String, y: String): String = s"($x + $y)"
    override def repeatedConvolve(n: Int, x: String): String = s"($n $x)"
    override def minus(x: String, y: String): String = s"($x - $y)"
    override def times(x: String, y: String): String = s"($x x $y)"
    override def quot(x: String, y: String): String = s"($x / $y)"
    override def negate(x: String): String = s"(-$x)"
    override def best(n: Int, xs: Iterable[String]): String = s"($xs b $n)"
    override def worst(n: Int, xs: Iterable[String]): String = s"($xs w $n)"

    override def fromValues(as: Iterable[A]): String = setString(as)
    override def rangedValues(max: A): String = s"d$max"
    override def rangedValues(min: A, max: A): String = s"d{$min-$max}"

    private[this] def setString(as: Iterable[A]): String = as.mkString("{", ", ", "}")
  }
  def Stringifier[A]: Stringifier[A] = new Stringifier[A] {}


  trait DefaultEvaluator[A] extends Evaluator[A] {

    override def repeatedConvolve(n: Int, x: R): R = {
      require(n > 0, s"repeatedConvolution must hapeen more than zero times, but got $n, when convolving over $x")
      Seq.fill(n)(x).reduce(convolve)
    }

  }

  trait ReflexiveMonoidal[A] extends DefaultEvaluator[A] {
    def monoid: Monoid[R]

    override def convolve(x: R, y: R): R = monoid.concat(x, y)
  }

  trait ReflexiveOrdered[A] extends ReflexiveMonoidal[A] with OrderedEvaluator[A] {

    override type R = A

    def ordering: Ordering[A]

    override def best(n: Int, xs: Iterable[A]): A =
      monoid.reduce(xs.toSeq.sorted(ordering).takeRight(n))

    override def worst(n: Int, xs: Iterable[A]): A =
      monoid.reduce(xs.toSeq.sorted(ordering).take(n))
  }

  trait ReflexiveNumeric[A] extends ReflexiveOrdered[A] with NumericEvaluator[A] {
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

  trait RandomReflexive[A] extends DefaultEvaluator[A] {

    override type R = A

    def random: java.util.Random

    override def fromValues(as: Iterable[A]): A = as.toSeq(random.nextInt(as.size))
  }

  class RandomNumericReflexive[A](implicit override val numeric: Integral[A]) extends ReflexiveNumeric[A] with RandomReflexive[A] {
    override def random: Random = new java.util.Random()
  }
  def RandomNumericReflexive[A](implicit numeric: Integral[A]) = new RandomNumericReflexive()

  trait MemoryMapContext[A, I] extends MemoryContext[A, I] {

    val memoryMap: scala.collection.mutable.Map[I, R] = scala.collection.mutable.Map.empty[I, R]

    override def store(id: I, value: R): R = {
      memoryMap(id) = value
      value
    }

    override def fetch(id: I): R = memoryMap(id)

    override def toString: String = s"MemoryMapContext(memoryMap = $memoryMap)"
  }

  trait UUIDMemoryContext[A] extends MemoryMapContext[A, java.util.UUID]

  trait StringMemoryContext[A] extends MemoryMapContext[A, String]

}