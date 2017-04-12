package net.paploo.diestats.statistics.domain

/**
  * Typeclass to define operations on a domain that must be accessible to  the code base to function.
  * @tparam A The domain type.
  */
trait DomainOperations[A] {

  /**
    * Concatenates two values of of type A.
    *
    * For example, for a Set this is union, and for numerical values this is addition.
    *
    * @return The contactenation of the first and second argument.
    */
  def concat(x: A, y: A): A

}


object DomainOperations {

  trait IntegerDomainOperations extends DomainOperations[Int] {
    override def concat(x: Int, y: Int): Int = x + y
  }

  object IntegerDomainOperations {

    def apply(): IntegerDomainOperations = intDomainOperations

    def fill(domain: Seq[Int])(implicit ord: Ordering[Int]): Seq[Int] =
      Range.inclusive(domain.min, domain.max)

    def span(domainA: Seq[Int], domainB: Seq[Int])(implicit ord: Ordering[Int]): Seq[Int] = {
      val min = ord.min(domainA.min, domainB.min)
      val max = ord.max(domainA.max, domainB.max)
      Range.inclusive(min, max)
    }

    private[this] val intDomainOperations: IntegerDomainOperations = new IntegerDomainOperations {}
  }

  trait SetDomainOperations[A] extends DomainOperations[Set[A]] {
    override def concat(x: Set[A], y: Set[A]): Set[A] = x union y
  }

  object SetDomainOperations {

    def apply[A](): SetDomainOperations[A] = new SetDomainOperations[A] {}

    def fill[A](domain: Seq[Set[A]])(implicit ord: Ordering[Set[A]]): Seq[Set[A]] =
      domain.sorted

    def span[A](domainA: Seq[Set[A]], domainB: Seq[Set[A]])(implicit ord: Ordering[Set[A]]): Seq[Set[A]] =
      (domainA ++ domainB).distinct.sorted

  }

  trait Implicits {

    implicit def intDomainOperations: DomainOperations[Int] = IntegerDomainOperations()

    implicit def setDomainOperations[A]: DomainOperations[Set[A]] = SetDomainOperations()

  }

  object Implicits extends Implicits

}
