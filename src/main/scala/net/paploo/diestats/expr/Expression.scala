package net.paploo.diestats.expr

import net.paploo.diestats.statistics.domain.DomainOps
import net.paploo.diestats.statistics.frequency.Frequency
import net.paploo.diestats.statistics.pdf.{PDF, PDFAble}
import net.paploo.diestats.statistics.util.{DistributionStatistics, NumericDistributionStatistics, Probability, StatisticalDistribution}

/**
  * Base trait for expressions over a domain of type A.
  *
  * Expressions define an AST for forming values over a domain A, with evaluation of operations
  * based withheld until an appropriate evaluation context is applied to generate a result.
  *
  * Example contexts include evalutation to PDFs, Statistics, or Randomly Generated Values.
  *
  * @tparam A
  * @tparam R
  */
trait Expression[A, R] extends (ExpressionOps[A, R] => R) {

  def +(that: Expression[A, R]): Expression[A, R] = Expression.Plus(this, that)

  def -(that: Expression[A, R]): Expression[A, R] = Expression.Minus(this, that)

  def *(that: Expression[A, R]): Expression[A, R] = Expression.Times(this, that)

  def /(that: Expression[A, R]): Expression[A, R] = Expression.Div(this, that)

  def unary_- : Expression[A, R] = Expression.Negate(this)

  def repeat(times: Int) = Expression.Repeat(this, times)

}

object Expression {

  case class Plus[A, R](x: Expression[A, R], y: Expression[A, R]) extends Expression[A, R] {
    override def apply(ops: ExpressionOps[A, R]): R = ops.plus(x, y)
  }

  case class Minus[A, R](x: Expression[A, R], y: Expression[A, R]) extends Expression[A, R] {
    override def apply(ops: ExpressionOps[A, R]): R = ops.minus(x, y)
  }

  case class Times[A, R](x: Expression[A, R], y: Expression[A, R]) extends Expression[A, R] {
    override def apply(ops: ExpressionOps[A, R]): R = ops.times(x, y)
  }

  case class Div[A, R](x: Expression[A, R], y: Expression[A, R]) extends Expression[A, R] {
    override def apply(ops: ExpressionOps[A, R]): R = ops.div(x, y)
  }

  case class Negate[A, R](x: Expression[A, R]) extends Expression[A, R] {
    override def apply(ops: ExpressionOps[A, R]): R = ops.negate(x)
  }

  case class Repeat[A, R](x: Expression[A, R], times: Int) extends Expression[A, R] {
    override def apply(ops: ExpressionOps[A, R]): R = ops.repeat(times, x)
  }

  case class TakeLower[A, R](n: Int, exprs: Iterable[Expression[A, R]]) extends Expression[A, R] {
    override def apply(ops: ExpressionOps[A, R]): R = ops.takeLower(n , exprs)
  }

}

/**
  * Trait expressing an implementation of operations over expressions on domain A,
  * returning value R.
  *
  * Example concrete types for R include a PDF for analysis, or even A in random value generation.
  *
  * ExpressionOps contains a mixture of operations that are expected, but only a
  * subset can be done on some domains. In the current design, it is up to the
  * implementation do decide how to handle non-sensical operations for a given
  * domain. For example, some consumers of this library may decide to throw
  * exceptions, while others may parameterize R over an Either or Try, while
  * others may make specialized validation ExpressionOps implementations for
  * a domain.
  * @tparam A The domain
  * @tparam R The operation aggregation value.
  */
trait ExpressionOps[A, R] {

  def plus(x: Expression[A, R], y: Expression[A, R]): R

  def minus(x: Expression[A, R], y: Expression[A, R]): R

  def times(X: Expression[A, R], y: Expression[A, R]): R

  def div(X: Expression[A, R], y: Expression[A, R]): R

  def negate(x: Expression[A, R]): R

  def repeat(times: Int, x: Expression[A, R]): R
  
  def takeLower(n: Int, exprs: Iterable[Expression[A, R]]): R
  
  def takeUpper(n: Int, exprs: Iterable[Expression[A, R]]): R

}


//TODO: This may not be that interesting, instead letting ExpressionOps take care of it?
//TODO: On the otherhand, I could see a consumer of PDFs wanting this sort of thing.
trait PDFOps[A] {

  def plus(x: PDF[A], y: PDF[A]): PDF[A]

  def minus(x: PDF[A], y: PDF[A]): PDF[A]

  def times(X: PDF[A], y: PDF[A]): PDF[A]

  def div(X: PDF[A], y: PDF[A]): PDF[A]

  def negate(x: PDF[A]): PDF[A]

  def repeat(times: Int, x: PDF[A]): PDF[A]

  def takeLower(n: Int, pdfs: Iterable[PDF[A]]): PDF[A]

  def takeUpper(n: Int, pdfs: Iterable[PDF[A]]): PDF[A]

}