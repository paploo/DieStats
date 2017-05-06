package net.paploo.diestats.expression.ast

/**
  * Used as a witness to the type of A in cases where not enough information is otherwise necessary.
  *
  * For example, `Expression.Fetch("id")` cannot infer the domain type for the expression, but we
  * cannot only give the type for the domain, but instead also have to pin down the complex evaluator
  * type, so we instead supply  witness: `Expression.Fetch("id")(TypeWitness[Foo])` for type `Foo`.
  *
  * Futhermore, most uses of the type witnesss use an implicit argument, therefore a single instance need
  * only be brought into implicit scope and it'll be used:
  * {{{
  *   implicit val witness = DomainType[Foo]
  *   Exression.Fetch("id")
  * }}}
  *
  * `TypeTag` could also be utilized, though it wouldn't be implicit
  * @tparam A
  */
trait DomainType[A]

object DomainType {
  def apply[A]: DomainType[A] = new DomainType[A] {}
}