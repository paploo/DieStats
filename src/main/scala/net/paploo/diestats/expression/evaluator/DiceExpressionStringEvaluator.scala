package net.paploo.diestats.expression.evaluator

class DiceExpressionStringEvaluator[A] extends NumericEvaluator[A, String] with StringMemoryEvaluator[A, String] {
  override def convolve(x: String, y: String): String = s"($x + $y)"
  override def repeatedConvolve(n: Int, x: String): String = s"($n $x)"

  override def minus(x: String, y: String): String = s"($x - $y)"
  override def times(x: String, y: String): String = s"($x x $y)"
  override def quot(x: String, y: String): String = s"($x / $y)"
  override def negate(x: String): String = s"(-$x)"

  override def best(n: Int, xs: Iterable[String]): String = s"(${exprsString(xs)} b $n)"
  override def worst(n: Int, xs: Iterable[String]): String = s"(${exprsString(xs)} w $n)"

  override def fromValues(as: Iterable[A]): String = valuesString(as)
  override def rangedValues(max: A): String = s"d$max"
  override def rangedValues(min: A, max: A): String = s"d{$min-$max}"

  override def store(id: String, value: String): String = s"(let $id = $value)"
  override def fetch(id: String): String = s"[$id]"

  private[this] def valuesString(as: Iterable[A]): String = as.mkString("{", ", ", "}")
  private[this] def exprsString(exprs: Iterable[String]): String = exprs.mkString("List(", ", ", ")")
}

object DiceExpressionStringEvaluator {

  def apply[A]: DiceExpressionStringEvaluator[A] = new DiceExpressionStringEvaluator[A]()

}
