package net.paploo.diestats.statistics.distribution

trait Distribution[A] extends (A => Double) {

  def domain: Seq[A]

  def pairs: Iterable[(A, Double)]

}