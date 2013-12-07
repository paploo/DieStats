package net.paploo.diestats.app

import net.paploo.diestats.pdf.{Histogram, PDF}

object App {

  def main(args: Array[String]): Unit = {
    val hist = new Histogram[Int]()
    hist << 6 << 6 << 7
    hist << 8 << 8 << 2 << 8

    println(hist)
    for (i <- 1 until 10) yield println((i, hist(i)))

    val pdf = hist.toPDF
    //val pdf = PDF.fromMap(hist.toMap)
    println(pdf)

    for( i <- pdf ) yield println(i)
    println(pdf(4))
    val xs = pdf.take(6)
  }

}