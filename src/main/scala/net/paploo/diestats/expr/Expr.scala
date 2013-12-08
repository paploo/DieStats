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

trait Expr {

  def +(expr: Expr): Expr = Expr(toPDF) + expr

  def -(expr: Expr): Expr = this + (-expr)

  def *(a: Int) = List.fill[Expr](a)(this).reduce(_ + _)

  def +(c: Int): Expr = this + Mod(c)

  def -(c: Int): Expr = this - Mod(c)

  def unary_- : Expr = {
    val pdf = toPDF.map {
      case (k, v) => (-k, v)
    }
    PDFExpr(pdf)
  }

  def min = toPDF.minKey

  def max = toPDF.maxKey

  def mean = toPDF.meanKey

  def median = toPDF.medianKey

  def modes = toPDF.modeKeys

  def toPDF: PDF

}

case class PDFExpr(pdf: PDF) extends Expr {
  override def +(expr: Expr): Expr = PDFExpr(toPDF compose expr.toPDF)

  override lazy val toPDF = pdf
}

case class Die(sides: Int) extends Expr {
  assert(sides > 0)

  override lazy val toPDF = (1 to sides).map((_, 1)).toSeq.toPDF

}

case class Mod(mod: Int) extends Expr {
  override lazy val toPDF = Seq((mod, 1)).toPDF
}
