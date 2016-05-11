package net.paploo.diestats.statistics.frequency

import net.paploo.diestats.statistics.distribution.PDF

trait Frequency[A] {
  def domain: Iterable[A]

  def add(a: A): Frequency[A] = add(a, 1)

  def add(a: A, delta: Long): Frequency[A]

  def ingest(seq: Traversable[A]): Frequency[A] = {
    seq.foreach(add)
    this
  }

  def ingestCounts(seq: Traversable[(A, Long)]): Frequency[A] = {
    seq.foreach {case (a, count) => add(a, count)}
    this
  }

  def counts: Long

  def toPDF: PDF[A]
}
