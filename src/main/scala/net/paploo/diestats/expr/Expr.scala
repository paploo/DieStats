package net.paploo.diestats.expr

import net.paploo.diestats.pdf.PDF
import net.paploo.diestats.pdf.PDFSeq._

object Expr {
  def apply(pdf: PDF): Expr = PDFExpr(pdf)

  def apply(mod: Int): Expr = Mod(mod)

  lazy val d2 = Die(2)
  lazy val d4 = Die(4)
  lazy val d6 = Die(6)
  lazy val d8 = Die(8)
  lazy val d10 = Die(10)
  lazy val d12 = Die(12)
  lazy val d20 = Die(20)
  lazy val dp = Die(100) - 1
}

/**
 * Defines the operations available at the given node of a dice expression.
 */
trait Expr {

  def +(expr: Expr): Expr = Expr(toPDF) + expr

  def -(expr: Expr): Expr = this + (-expr)

  def *(a: Int) = List.fill[Expr](a)(this).reduce(_ + _)

  def +(c: Int): Expr = this + Mod(c)

  def -(c: Int): Expr = this - Mod(c)

  def unary_- : Expr = {
    val pdf = toPDF.map {case (k, v) => (-k, v)}
    PDFExpr(pdf)
  }

  def min = toPDF.minKey

  def max = toPDF.maxKey

  def mean = toPDF.meanKey

  def median = toPDF.medianKey

  def modes = toPDF.modeKeys

  def toPDF: PDF

}

/**
 * Encapsulates a raw [[PDF]] as an [[Expr]].
 *
 * This is frequently used to hold the composite [[PDF]] from reduction of more
 * simple expressions.
 * @param pdf The [[PDF]] to encapsulate.
 */
case class PDFExpr(pdf: PDF) extends Expr {
  override def +(expr: Expr): Expr = PDFExpr(toPDF compose expr.toPDF)

  override lazy val toPDF = pdf
}

/**
 * [[Expr]] for a single Die whose sides are 1 through [[sides]] inclusive.
 * @param sides The number of sides.
 */
case class Die(sides: Int) extends Expr {
  assert(sides > 0)

  override lazy val toPDF = (1 to sides).map((_, 1)).toSeq.toPDF

}

/**
 * An [[Expr]] for a single modifier value.
 * @param modifier The value of the modifier.
 */
case class Mod(modifier: Int) extends Expr {
  override lazy val toPDF = Seq((modifier, 1)).toPDF
}
