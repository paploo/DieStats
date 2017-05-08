package net.paploo.diestats.expression.evaluator

/**
  * Trait for an Evaluator that retains the contextual information of
  * a memory.
  *
  * Implementations require mutable state!
  *
  * Storage is expected to return the stored value, e.g.:
  * store(id, value) should === (fetch(id))
  *
  * @tparam A The domain type
  * @tparam R The evaluation result type
  * @tparam I The ID type
  */
trait MemoryEvaluator[A, R, I] extends Evaluator[A, R] {
  def store(id: I, value: R): R
  def fetch(id: I): R
}

object MemoryEvaluator {

  /**
    * A base implementation around a mutable.Map, that can be mixed-in to other evaluators.
    * @tparam A The domain type
    * @tparam R The evaluation result type
    * @tparam I The ID type
    */
  trait MemoryMapEvaluator[A, R, I] extends MemoryEvaluator[A, R, I] {

    val memoryMap: scala.collection.mutable.Map[I, R] = scala.collection.mutable.Map.empty[I, R]

    override def store(id: I, value: R): R = {
      memoryMap(id) = value
      value
    }

    override def fetch(id: I): R = memoryMap(id)

    override def toString: String = s"MemoryMapContext(memoryMap = $memoryMap)"
  }

}

/**
  * A trait for the most common kind of memory context.
  *
  * This is necessary because the scala type inference engine is
  * not very good, and can't do the type gymnastics to resolve
  * the type of Evaluator needed for an expression that admits
  * the ID as a type parameter, unless we give it more explicit typing.
  * As it turns out, not even type-aliases cover it, but instead a more
  * specific reduction of type is needed!
  *
  * @tparam A The domain type
  * @tparam R The evaluation result type
  */
trait StringMemoryEvaluator[A, R] extends MemoryEvaluator[A, R, String]

object StringMemoryEvaluator {

  /**
    * A base implementation around a mutable.Map, that can be mixed-in to other evaluators.
    * @tparam A The domain type
    * @tparam R The evaluation result type
    */
  trait StringMemoryMapEvaluator[A, R] extends MemoryEvaluator.MemoryMapEvaluator[A, R, String] with StringMemoryEvaluator[A, R]

}
