package net.paploo.diestats.test

import net.paploo.diestats.test.Coll.{Bar, Baz, Foo}

import scala.collection.generic.{CanBuildFrom, GenTraversableFactory, GenericCompanion, GenericTraversableTemplate}
import scala.collection.{GenTraversable, TraversableLike, mutable}

class TraversableTest extends SpecTest {

  describe("foo") {

    import Foo.Implicits._

    val foo = new Foo(Seq(10,50,100,500))

    it("should filter") {

      val f: Foo[Int] = foo.filter(_ >= 100)

      f.toSeq should === (Seq(100, 500))

    }

    it("should map") {

      val f: Foo[Int] = foo.map(_+1)

      f.toSeq should === (Seq(11, 51, 101, 501))

    }

  }

  describe("bar") {

    import Bar.Implicits._

    val bar = new Bar(Seq(10,50,100,500))

    it("should filter") {

      val b: Bar[Int] = bar.filter(_ >= 100)

      b.toSeq should === (Seq(100, 500))

    }

    it("should map") {

      val b: Bar[Int] = bar.map(_+1)

      b.toSeq should === (Seq(11, 51, 101, 501))

    }

  }

  describe("baz") {

    import Baz.Implicits._

    val baz = new Baz(Seq(10,50,100,500))

    it("should filter") {

      val b: Baz[Int] = baz.filter(_ >= 100)

      b.toSeq should === (Seq(100, 500))

    }

    it("should map") {

      val b: Baz[Int] = baz.map(_+1)

      b.toSeq should === (Seq(11, 51, 101, 501))

    }

  }

}


object Coll {

  //Foo implements all the pieces manually.

  class Foo[A](values: Seq[A]) extends Traversable[A] with TraversableLike[A, Foo[A]] {
    override def foreach[U](f: (A) => U): Unit = values.foreach(f)

    override protected[this] def newBuilder: mutable.Builder[A, Foo[A]] = Foo.newBuilder[A]
  }

  object Foo {

    def newBuilder[A]: mutable.Builder[A, Foo[A]] = new mutable.Builder[A, Foo[A]] {
      private[this] val buf = mutable.ArrayBuffer.empty[A]

      override def +=(elem: A): this.type = {
        buf += elem
        this
      }

      override def clear(): Unit = buf.clear()

      override def result(): Foo[A] = new Foo(buf)
    }

    def canBuildFrom[A]: CanBuildFrom[TraversableOnce[A], A, Foo[A]] = new CanBuildFrom[TraversableOnce[A], A, Foo[A]] {
      override def apply(from: TraversableOnce[A]): mutable.Builder[A, Foo[A]] = newBuilder[A]

      override def apply(): mutable.Builder[A, Foo[A]] = newBuilder[A]
    }

    object Implicits {
      implicit def fooCanBuildFrom[A]: CanBuildFrom[TraversableOnce[A], A, Foo[A]] = Foo.canBuildFrom
    }

  }

  //Bar uses the GenericTraversableTemplate[A, Bar] to help out, which requires using GenericCompanion[Bar] on the companion class.

  class Bar[A](values: Seq[A]) extends Traversable[A] with TraversableLike[A, Bar[A]] with GenericTraversableTemplate[A, Bar] {
    override def foreach[U](f: (A) => U): Unit = values.foreach(f)

    override def companion: GenericCompanion[Bar] = Bar
  }

  object Bar extends GenericCompanion[Bar] {

    override def newBuilder[A]: mutable.Builder[A, Bar[A]] = new mutable.Builder[A, Bar[A]] {
      private[this] val buf = mutable.ArrayBuffer.empty[A]

      override def +=(elem: A): this.type = {
        buf += elem
        this
      }

      override def clear(): Unit = buf.clear()

      override def result(): Bar[A] = new Bar(buf)
    }

    def canBuildFrom[A]: CanBuildFrom[TraversableOnce[A], A, Bar[A]] = new CanBuildFrom[TraversableOnce[A], A, Bar[A]] {
      override def apply(from: TraversableOnce[A]): mutable.Builder[A, Bar[A]] = newBuilder[A]

      override def apply(): mutable.Builder[A, Bar[A]] = newBuilder[A]
    }

    object Implicits {
      implicit def fooCanBuildFrom[A]: CanBuildFrom[TraversableOnce[A], A, Bar[A]] = Bar.canBuildFrom
    }

  }

  class Baz[A](values: Seq[A]) extends Traversable[A] with TraversableLike[A, Baz[A]] with GenericTraversableTemplate[A, Baz] {
    override def foreach[U](f: (A) => U): Unit = values.foreach(f)

    override def companion: GenericCompanion[Baz] = Baz
  }

  object Baz extends GenTraversableFactory[Baz] {

    override def newBuilder[A]: mutable.Builder[A, Baz[A]] = new mutable.Builder[A, Baz[A]] {
      private[this] val buf = mutable.ArrayBuffer.empty[A]

      override def +=(elem: A): this.type = {
        buf += elem
        this
      }

      override def clear(): Unit = buf.clear()

      override def result(): Baz[A] = new Baz(buf)
    }

    def canBuildFrom[A]: CanBuildFrom[Baz[A], A, Baz[A]] = new GenericCanBuildFrom[A]

    object Implicits {
      implicit def fooCanBuildFrom[A]: CanBuildFrom[Baz[A], A, Baz[A]] = Baz.canBuildFrom
    }

  }

}
