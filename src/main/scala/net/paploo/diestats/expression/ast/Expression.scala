package net.paploo.diestats.expression.ast

import net.paploo.diestats.expression.evaluator.{Evaluator, NumericEvaluator, OrderedEvaluator, StringMemoryEvaluator}

/**
  * Expression encapsulates a node in the AST over a domain A.
  *
  * Expressions may be evaluated with a variety of Evaluator
  * implementations, which may do different things. For example,
  * one may give a human readable string form of the expression,
  * while another evaluates with a random number generator, and
  * yet another evaluates as a ProbabilityDistribution[A].
  *
  * The evaluator type is specified using a higher-kinded type
  * to facilitate bounds imposed by the various concrete expression
  * nodes. For example, Convolve should work with any evaluator, whereas
  * Minus may only be evaluated by a numerical evaluator. Making the
  * evaluator be a bounded type parameter ensures that composition
  * of the nodes retains the most restrictive evaluator.
  *
  * One downside is that the scala 2.12 type system is easily confused
  * with "caking" together multiple Evaluator traits when any of them take
  * extra type parameters, even if they are bound via type alias, when used
  * as a type bound; the workaround is to make specific traits that pin
  * it back down to only the core type parameters, and then things
  * work again.
  *
  * @tparam A
  * @tparam E
  */
trait Expression[A, -E[X,Y] <: Evaluator[X,Y]] {
  def apply[R](e: E[A,R]): R
}

object Expression {

  /**
    * Given a bunch of expressions, evaluate them all in-order, independently, and then
    * return the result of the last one.
    */
  case class Statements[A, -E[X,Y] <: Evaluator[X,Y]](xs: Expression[A, E]*) extends Expression[A, E] {
    override def apply[R](e: E[A, R]): R = xs.map(_.apply(e)).last
  }


  case class Values[A, -E[X,Y] <: Evaluator[X,Y]](values: A*) extends Expression[A, E] {
      override def apply[R](e: E[A,R]): R = e.fromValues(values)
  }


  case class Convolve[A, -E[X,Y] <: Evaluator[X,Y]](x: Expression[A, E], y: Expression[A, E]) extends Expression[A, E] {
    override def apply[R](e: E[A,R]): R = e.convolve(x(e), y(e))
  }

  case class RepeatedConvolve[A, -E[X,Y] <: Evaluator[X,Y]](n: Int, x: Expression[A, E]) extends Expression[A, E] {
    override def apply[R](e: E[A, R]): R = e.repeatedConvolve(n, x(e))
  }


  case class Best[A, -E[X,Y] <: OrderedEvaluator[X,Y]](n: Int, xs: Iterable[Expression[A, E]]) extends Expression[A, E] {
    override def apply[R](e: E[A, R]): R = e.best(n, xs.map(_.apply(e)))
  }

  case class Worst[A, -E[X,Y] <: OrderedEvaluator[X,Y]](n: Int, xs: Iterable[Expression[A, E]]) extends Expression[A, E] {
    override def apply[R](e: E[A, R]): R = e.worst(n, xs.map(_.apply(e)))
  }


  case class Plus[A, -E[X,Y] <: NumericEvaluator[X,Y]](x: Expression[A, E], y: Expression[A, E]) extends Expression[A, E] {
    override def apply[R](e: E[A,R]): R = e.plus(x(e), y(e))
  }

  case class Minus[A, -E[X,Y] <: NumericEvaluator[X,Y]](x: Expression[A, E], y: Expression[A, E]) extends Expression[A, E] {
    override def apply[R](e: E[A,R]): R = e.minus(x(e), y(e))
  }

  case class Mult[A, -E[X,Y] <: NumericEvaluator[X,Y]](x: Expression[A, E], y: Expression[A, E]) extends Expression[A, E] {
    override def apply[R](e: E[A,R]): R = e.times(x(e), y(e))
  }

  case class Quot[A, -E[X,Y] <: NumericEvaluator[X,Y]](x: Expression[A, E], y: Expression[A, E]) extends Expression[A, E] {
    override def apply[R](e: E[A,R]): R = e.quot(x(e), y(e))
  }

  case class Negate[A, -E[X,Y] <: NumericEvaluator[X,Y]](x: Expression[A, E]) extends Expression[A, E] {
    override def apply[R](e: E[A,R]): R = e.negate(x(e))
  }

  case class DieValue[A, -E[X,Y] <: NumericEvaluator[X,Y]](numberOfSides: A) extends Expression[A, E] {
    override def apply[R](e: E[A,R]): R = e.rangedValues(numberOfSides)
  }

  case class RangedValue[A, -E[X,Y] <: NumericEvaluator[X,Y]](min: A, max: A) extends Expression[A, E] {
    override def apply[R](e: E[A,R]): R = e.rangedValues(min, max)
  }

  case class Store[A, -E[X,Y] <: StringMemoryEvaluator[X,Y]](id: String, x: Expression[A, E]) extends Expression[A, E] {
    override def apply[R](e: E[A, R]): R = e.store(id, x(e))
  }

  /**
    * Fetches the expression for the given id.
    *
    * By itself, there is no information for the compiler to pin down the type
    * of A, but we cannot partially apply the time (e.g. `Fetch[String, _]("foo")` if A =:= String),
    * so we use a witness class as a second argument that can pin the type parameter down.
    *
    * If an unambiguous witness is implicitly in scope, then it will be used to pin down the domain type.
    *
    * @param id
    * @param witness A value of type A that is used to pin down type A without having to give an explicit type E.
    * @tparam A
    * @tparam E
    */
  case class Fetch[A, -E[X,Y] <: StringMemoryEvaluator[X,Y]](id: String)(implicit witness: DomainType[A]) extends Expression[A, E] {
    override def apply[R](e: E[A, R]): R = e.fetch(id)
  }

}

