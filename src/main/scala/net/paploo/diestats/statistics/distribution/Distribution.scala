package net.paploo.diestats.statistics.distribution

trait Distribution[A] extends (A => Double) {

  def get(a: A): Option[Double]

  override def apply(a: A): Double = get(a).getOrElse(0.0)

  def domainSet: Set[A]

}

trait StrictlyOrderedDistribution[A] extends Distribution[A] {

  def ordering: Ordering[A]

  def domain: Seq[A]

  def pairs: Iterable[(A, Double)]

}

trait LazilyOrderedDistribution[A] extends Distribution[A] {

  def domain(implicit ord: Ordering[A]): Seq[A]

  def pairs(implicit ord: Ordering[A]): Iterable[(A, Double)]

}