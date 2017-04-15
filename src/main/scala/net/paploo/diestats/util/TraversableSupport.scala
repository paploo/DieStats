package net.paploo.diestats.util

import scala.collection.generic.CanBuildFrom
import scala.collection.mutable
import scala.language.higherKinds

object TraversableSupport {

  trait SequenceBufferBuilder[A, +M[_]] extends mutable.Builder[A, M[A]] {

    def constructResult(buffer: Traversable[A]): M[A]

    private[this] def buffer: mutable.Buffer[A] = new mutable.ListBuffer[A]()

    override def +=(elem: A): SequenceBufferBuilder.this.type = {
      buffer += elem
      this
    }

    override def clear(): Unit = buffer.clear()

    override def result(): M[A] = constructResult(buffer)
  }

  object SequenceBufferBuilder {

    def apply[A, M[_]](f: TraversableOnce[A] => M[A]): mutable.Builder[A, M[A]] = new SequenceBufferBuilder[A, M] {
      override def constructResult(buffer: Traversable[A]): M[A] = f(buffer)
    }

  }

  trait CanBuildFromTraversable[A, M[_]] extends CanBuildFrom[TraversableOnce[A], A, M[A]] {

    def newBuilder: mutable.Builder[A, M[A]]

    override def apply(from: TraversableOnce[A]): mutable.Builder[A, M[A]] = newBuilder

    override def apply(): mutable.Builder[A, M[A]] = newBuilder
  }

  object CanBuildFromTraversable {

    def apply[A, M[_]](builder: mutable.Builder[A, M[A]]): CanBuildFrom[TraversableOnce[A], A, M[A]] = new CanBuildFromTraversable[A, M] {
      override val newBuilder: mutable.Builder[A, M[A]] = builder
    }

  }

}
