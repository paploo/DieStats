//package net.paploo.diestats.test
//
//class VarianceTest {
//
//  trait Cov[+A] {
//
//    def get[B >: A](b: B): A
//
//    def +[B >: A](b: B): Cov[B]
//
//    def ++[B >: A](other: TraversableOnce[B]): Cov[B]
//
//  }
//
//  object Cov {
//
//    class CovImpl[+A](val map: Map[A, Long]) extends Cov[A] {
//
//      override def get[B >: A](b: B): A = map(b)
//
//      override def +[B >: A](b: B): Cov[B] = new CovImpl(map + (b -> 1))
//
//      override def ++[B >: A](other: TraversableOnce[B]): Cov[B] = ???
//    }
//
//    def apply[A](map: Map[A, Long]) = new CovImpl[A](map)
//
//  }
//
//  trait Contra[-A] {
//
//    def get[B <: A](a: A): B
//
//    def +[B <: A](b: B): Cov[B]
//
//    def ++[B <: A](other: TraversableOnce[B]): Cov[B]
//
//
//  }
//
//  object Contra {
//
//    class ContraImpl[-A] extends Contra[A]
//
//    def apply[A]() = new ContraImpl[A]
//
//  }
//
//  class Animal(species: String)
//  class Dog(species: String, kind: String) extends Animal(species)
//  class Cat(species: String, color: String) extends Animal(species)
//
//  object Foo {
//
//    def x: Cov[Animal] = Cov[Dog]()
//    //def y: Cov[Dog] = Cov[Animal]() Nope!
//
//    //def a: Contra[Animal] = Contra[Dog]() Nope!
//    def b: Contra[Dog] = Contra[Animal]()
//
//  }
//
//}
