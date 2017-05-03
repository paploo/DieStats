package net.paploo.diestats.expression.ast

import net.paploo.diestats.expression.ast.Expression.{Convolve, Plus, Values}

import scala.language.higherKinds

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
  * @tparam A
  * @tparam E
  */
trait Expression[A, -E[X,Y] <: Evaluator[X,Y]] {
  def apply[R](e: E[A,R]): R
}

object Expression {

  case class Values[A, -E[X,Y] <: Evaluator[X,Y]](values: A*) extends Expression[A, E] {
      override def apply[R](e: E[A,R]): R = e.fromValues(values)
  }

  case class Convolve[A, -E[X,Y] <: Evaluator[X,Y]](x: Expression[A, E], y: Expression[A, E]) extends Expression[A, E] {
    override def apply[R](e: E[A,R]): R = e.convolve(x(e), y(e))
  }

  case class Plus[A, -E[X,Y] <: NumericEvaluator[X,Y]](x: Expression[A, E], y: Expression[A, E]) extends Expression[A, E] {
    override def apply[R](e: E[A,R]): R = e.plus(x(e), y(e))
  }

}

object Runner {

  def main(args: Array[String]): Unit = {

    val foo: Expression[String, Evaluator] = Convolve(Values("foo", "bar"), Values("alpha", "beta"))
    val resultFoo: String = foo.apply(Evaluator.Stringifier)
    println(resultFoo)

    val bar: Expression[Int, NumericEvaluator] = Plus(Values(1,2), Values(3,4))
    val resultBar: Int = bar.apply(Evaluator.RandomNumericReflexive)
    println(resultBar)

    //Shouldn't compile because plus requires NumericDomainEvaluator.
    //val bar2: Expression[Int, Evaluator] = Plus(Values(1,2), Values(3,4))

    //We can be over-sepcific too:
    val fooN: Expression[String, NumericEvaluator] = foo


    val cfoo: Expression[String, Evaluator] = Convolve(foo, foo)
    println(cfoo(Evaluator.Stringifier))

    //We don't want this to work, since bar requires NumericDomainEvaluator.
    //val cbar: Expression[Int, Evaluator] = Convolve(bar, bar)

    val cbar2: Expression[Int, NumericEvaluator] = Convolve(bar, bar)
    println(cbar2(Evaluator.RandomNumericReflexive))

  }

}

