package net.paploo.diestats.statistics

import net.paploo.diestats.statistics.util.Ordering
import net.paploo.diestats.statistics.util.{Monoid, Probability}

trait Implicits
  extends Probability.Implicits
  with Monoid.Implicits
  with Ordering.Implicits

object Implicits extends Implicits
