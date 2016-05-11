package net.paploo.diestats.statistics.distribution

trait Distribution[-A] extends (A => Double) {
  override def apply(v1: A): Double = ???

  //def domain: Seq[A]

  //def pairs: Iterable[(A, Double)]
}