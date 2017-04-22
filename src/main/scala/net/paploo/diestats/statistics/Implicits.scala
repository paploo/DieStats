package net.paploo.diestats.statistics

import net.paploo.diestats.statistics.domain.DomainOps
import net.paploo.diestats.statistics.util.{Monoid, Probability}

trait Implicits
  extends Probability.Implicits
  with Monoid.Implicits
  with DomainOps.Implicits

object Implicits extends Implicits
