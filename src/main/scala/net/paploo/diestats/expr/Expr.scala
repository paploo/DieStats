package net.paploo.diestats.expr

import net.paploo.diestats.pdf.PDF
import net.paploo.diestats.pdf.PDFSeq._

trait Expr {

 def pdf: PDF

}

case class Die(sides: Int) extends Expr {
  assert(sides > 0)

  def pdf: PDF = (1 to sides).map( (_,1) ).toSeq.toPDF

}
