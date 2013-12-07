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

    val pdf1 = PDF(2 -> 1, 3 -> 2, 4 -> 1)
    val pdf2 = PDF(1 -> 1, 2 -> 1)
    println(pdf1)
    println(pdf2)
    println(pdf1 compose pdf2)
  }

}