package net.paploo.diestats.expression.ast

import net.paploo.diestats.expression.ast.Expression.{Convolve, Plus, Values}

import scala.language.higherKinds

trait Evaluator[A, R] {
  def fromValues(as: Seq[A]): R
  def convolve(x: R, y: R): R
}

trait NumericEvaluator[A, R] extends Evaluator[A, R] {
  def plus(x: R, y: R): R
}

object Evaluator {

  trait StringEvaluator extends Evaluator[String, String] {
    override def fromValues(as: Seq[String]): String = as.toString
    override def convolve(x: String, y: String): String = s"$x convolve $y"
  }
  object StringEvaluator extends StringEvaluator

  object IntNumericEvaluator extends NumericEvaluator[Int, Int] {
    override def fromValues(as: Seq[Int]): Int = as.head

    override def convolve(x: Int, y: Int): Int = x+y

    override def plus(x: Int, y: Int): Int = convolve(x,y)
  }

}

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
    val resultFoo: String = foo.apply(Evaluator.StringEvaluator)
    println(resultFoo)

    val bar: Expression[Int, NumericEvaluator] = Plus(Values(1,2), Values(3,4))
    val resultBar: Int = bar.apply(Evaluator.IntNumericEvaluator)
    println(resultBar)

    //Shouldn't compile because plus requires NumericDomainEvaluator.
    //val bar2: Expression[Int, Evaluator] = Plus(Values(1,2), Values(3,4))


    val cfoo: Expression[String, Evaluator] = Convolve(foo, foo)

    //We don't want this to work, since bar requires NumericDomainEvaluator.
    //val cbar: Expression[Int, Evaluator] = Convolve(bar, bar)

    val cbar2: Expression[Int, NumericEvaluator] = Convolve(bar, bar)
    println(cbar2(Evaluator.IntNumericEvaluator))

  }

}


