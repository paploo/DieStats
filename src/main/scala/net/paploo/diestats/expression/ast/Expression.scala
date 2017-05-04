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
  * with layering of multiple Evaluator traits, especially
  * in cases where the arity doesn't match (e.g. with MemoryContext);
  * thus it sometimes needs hinting with explicit type parameters.
  *
  * One work-around in practical contexts is thatyou'll want to pin-down
  * the evaluator type using a type alias, and then. For example,
  * {{{
  *   type MyEvaluator[A, R] = NumericEvaluator[A, R] with MemoryContext[A, R, String]
  * }}}
  * However this design may be abandoned for something simpler, as this does
  * not allow for building an AST that can be evaluated later.
  *
  * The solutions into the future are to:
  * 1. Abandon the type-level programming enforcement in favor of runtime errors (ick), or
  * 2. Keep the evaluator hierarchy linear.
  * Neither of these are ideal.
  *
  * @tparam A
  * @tparam E
  */
trait Expression[A, -E[X] <: Evaluator[X]] {
  def apply[R](e: E[A]): e.R
}

object Expression {

  /**
    * Given a bunch of expressions, evaluate them all in-order, independently, and then
    * return the result of the last one.
    */
  case class Statements[A, -E[X] <: Evaluator[X]](xs: Iterable[Expression[A, E]]) extends Expression[A, E] {
    override def apply[R](e: E[A]): e.R = xs.map(_.apply(e)).last
  }


  case class Values[A, -E[X] <: Evaluator[X]](values: A*) extends Expression[A, E] {
      override def apply[R](e: E[A]): e.R = e.fromValues(values)
  }


  case class Convolve[A, -E[X] <: Evaluator[X]](x: Expression[A, E], y: Expression[A, E]) extends Expression[A, E] {
    override def apply[R](e: E[A]): e.R = e.convolve(x(e), y(e))
  }

  case class RepeatedConvolve[A, -E[X] <: Evaluator[X]](n: Int, x: Expression[A, E]) extends Expression[A, E] {
    override def apply[R](e: E[A]): e.R = e.repeatedConvolve(n, x(e))
  }


  case class Best[A, -E[X] <: OrderedEvaluator[X]](n: Int, xs: Iterable[Expression[A, E]]) extends Expression[A, E] {
    override def apply[R](e: E[A]): e.R = e.best(n, xs.map(_.apply(e)))
  }

  case class Worst[A, -E[X] <: OrderedEvaluator[X]](n: Int, xs: Iterable[Expression[A, E]]) extends Expression[A, E] {
    override def apply[R](e: E[A]): e.R = e.best(n, xs.map(_.apply(e)))
  }


  case class Plus[A, -E[X] <: NumericEvaluator[X]](x: Expression[A, E], y: Expression[A, E]) extends Expression[A, E] {
    override def apply[R](e: E[A]): e.R = e.plus(x(e), y(e))
  }

  case class Minus[A, -E[X] <: NumericEvaluator[X]](x: Expression[A, E], y: Expression[A, E]) extends Expression[A, E] {
    override def apply[R](e: E[A]): e.R = e.minus(x(e), y(e))
  }

  case class Mult[A, -E[X] <: NumericEvaluator[X]](x: Expression[A, E], y: Expression[A, E]) extends Expression[A, E] {
    override def apply[R](e: E[A]): e.R = e.times(x(e), y(e))
  }

  case class Quot[A, -E[X] <: NumericEvaluator[X]](x: Expression[A, E], y: Expression[A, E]) extends Expression[A, E] {
    override def apply[R](e: E[A]): e.R = e.quot(x(e), y(e))
  }

  case class Negate[A, -E[X] <: NumericEvaluator[X]](x: Expression[A, E]) extends Expression[A, E] {
    override def apply[R](e: E[A]): e.R = e.negate(x(e))
  }

  case class DieValue[A, -E[X] <: NumericEvaluator[X]](numberOfSides: A) extends Expression[A, E] {
    override def apply[R](e: E[A]): e.R = e.rangedValues(numberOfSides)
  }

  case class RangedValue[A, -E[X] <: NumericEvaluator[X]](min: A, max: A) extends Expression[A, E] {
    override def apply[R](e: E[A]): e.R = e.rangedValues(min, max)
  }

  case class Store[A, -E[X] <: MemoryContext[X,I], I](id: I, x: Expression[A, E]) extends Expression[A, E] {
    override def apply[R](e: E[A]): e.R = e.store(id, x(e))
  }

  case class Fetch[A, -E[X] <: MemoryContext[X,I], I](id: I) extends Expression[A, E] {
    override def apply[R](e: E[A]): e.R = e.fetch(id)
  }

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

    type NFoo[A] = NumericEvaluator[A] with MemoryContext[A, UUID]
    type NBar[A] = NumericEvaluator[A] with MemoryContext[A, String]

    def uuidMM(): NFoo[String] = new Evaluator.Stringifier[String] with Evaluator.MemoryMapContext[String, UUID] {}
    def stringMM(): NBar[String] = new Evaluator.Stringifier[String] with Evaluator.MemoryMapContext[String, String] {}
    //val stringMM = new Evaluator.Stringifier[String] with Evaluator.MemoryMapContext[String, String] {}

    val memory = Store(UUID.randomUUID(), foo)
    val mmu = uuidMM()
    memory.apply(mmu)
    println(mmu)
    //Doesn't compile because the mmeory type includes the UUID type and thus needs an NFoo
    //    val mms = stringMM()
    //    memory.apply(mms)
    //    println(mms)

    //Doesn't work, needs explicit typing to Expression[String, NFoo]; which defeats the
    //purpose of building the AST ahead of time.
    //val convolvMemory: Expression[String, NFoo] = Convolve(memory, foo)

    val convolvMemory: Expression[String, NFoo] = Convolve[String, NFoo](memory, foo)
    val mm = uuidMM()
    convolvMemory.apply(mm)
    println(mm)

    //To compile, we have to give the type of the sequence explicitly.
    val stats: Expression[String, NFoo] = Statements(Seq[Expression[String, NFoo]](
      Store(UUID.randomUUID(), foo),
      foo
    ))
    val mm2 = uuidMM()
    stats.apply(mm2)

    println(mm2)

  }

}

