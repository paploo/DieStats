package net.paploo.diestats.expression.ast

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
  * One downside is that the scala 2.12 type system is easily confused
  * with the composition of these parameterized traits, especially
  * in cases where the arity doesn't match. This can be helped
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
  case class Statements[A, -E[X,Y] <: Evaluator[X,Y]](xs: Iterable[Expression[A, E]]) extends Expression[A, E] {
    override def apply[R](e: E[A, R]): R =
      xs.map(_.apply(e)).last
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
    override def apply[R](e: E[A, R]): R = e.best(n, xs.map(_.apply(e)))
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

  case class Store[A, -E[X,Y] <: UUIDMemoryContext[X,Y]](id: java.util.UUID, x: Expression[A, E]) extends Expression[A, E] {
    override def apply[R](e: E[A,R]): R = e.store(id, x(e))
  }
//
//  case class Fetch[A, -E[X,Y] <: Expression[X,Y] with MemoryContext[X,Y,I], I](id: I) extends Expression[A, E] {
//    override def apply[R](e: E[A,R]): R = e.fetch(id)
//  }

  def foo[A, B <: A](x: A, y: A): B = ???
  trait Base
  trait Num extends Base
  trait Mem extends Base
  val x = foo(new Num {}, new Mem {})

}

object Runner {

  import net.paploo.diestats.expression.ast.Expression._
  import java.util.UUID

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

    type NFoo[A, R] = NumericEvaluator[A, R] with UUIDMemoryContext[A, R]

    val uuidMM: NFoo[String, String] = new Evaluator.Stringifier[String] with Evaluator.MemoryMapContext[String, String] {}
    //val stringMM = new Evaluator.Stringifier[String] with Evaluator.MemoryMapContext[String, String] {}

    val memory = Store(UUID.randomUUID(), foo)

    val convolvMemory: Expression[String, NFoo] = Convolve[String, NFoo](memory, foo)
    val convolvMemory2: Expression[String, NFoo] = Convolve(memory, foo)

    val stats: Expression[String, NFoo] = Statements(Seq(
      Store(UUID.randomUUID(), foo),
      foo
    ))
    stats.apply(uuidMM)

    println(uuidMM.memoryMap)

  }

}

