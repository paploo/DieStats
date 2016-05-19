package net.paploo.diestats.statistics.domain

trait DomainOperations[A] {

  def add(x: A, y: A): A

  def sub(x: A, y: A): A

  /**
    * Given a set of domain values, fill in any missing values, if any.
    *
    * Implementations should endeavor to return an ordered domain, if an ordering is reasonable.
    *
    * @return a domain with no missing values.
    */
  def fill(domain: Seq[A])(implicit ord: Ordering[A]): Seq[A]

  /**
    * Given two domains, give a filled domain that spans across them, inclusive.
    *
    * @return an ordered domain with no mi
    */
  def span(domainA: Seq[A], domainB: Seq[A])(implicit ord: Ordering[A]): Seq[A]

}

object DomainOperations {

  object DefaultDomainOperations {

    val intDomainOperations: DomainOperations[Int] = new DomainOperations[Int] {
      override def add(x: Int, y: Int): Int = x + y

      override def sub(x: Int, y: Int): Int = x - y

      override def fill(domain: Seq[Int])(implicit ord: Ordering[Int]): Seq[Int] =
        Range.inclusive(domain.min, domain.max)

      override def span(domainA: Seq[Int], domainB: Seq[Int])(implicit ord: Ordering[Int]): Seq[Int] = {
        val min = ord.min(domainA.min, domainB.min)
        val max = ord.max(domainA.max, domainB.max)
        Range.inclusive(min, max)
      }
    }

    def setDomainOperations[A]: DomainOperations[Set[A]] = new DomainOperations[Set[A]] {
      override def add(x: Set[A], y: Set[A]): Set[A] = x union y

      override def sub(x: Set[A], y: Set[A]): Set[A] = x diff y

      override def fill(domain: Seq[Set[A]])(implicit ord: Ordering[Set[A]]): Seq[Set[A]] =
        domain.sorted

      override def span(domainA: Seq[Set[A]], domainB: Seq[Set[A]])(implicit ord: Ordering[Set[A]]): Seq[Set[A]] =
        (domainA ++ domainB).distinct.sorted
    }

  }

  object Implicits {

    implicit def intDomainOperations: DomainOperations[Int] = DefaultDomainOperations.intDomainOperations

    implicit def setDomainOperations[A]: DomainOperations[Set[A]] = DefaultDomainOperations.setDomainOperations

  }

}
