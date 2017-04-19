package net.paploo.diestats.statistics.util

trait Monoid[A] {

  def concat(x: A, y: A): A

  def empty: A

}
