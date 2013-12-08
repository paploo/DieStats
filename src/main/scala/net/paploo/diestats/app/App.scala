package net.paploo.diestats.app

import net.paploo.diestats.pdf.{Histogram, PDF}
import net.paploo.diestats.expr._
import net.paploo.diestats.expr.Expr._
import net.paploo.diestats.pdf.PDFSeq._
import net.paploo.diestats.pdf.PDFSeq

object App {

  def main(args: Array[String]): Unit = {
    val ex = PDFExpr(PDF(1->1,2->3,3->3,4->1))
    println(ex.min)
    println(ex.max)
    println(ex.mean)
    println(ex.median)
    println(ex.modes)
  }
}