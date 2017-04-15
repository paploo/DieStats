package net.paploo.diestats.statistics

import net.paploo.diestats.statistics.frequency.Frequency

trait Implicits extends Probability.Implicits with Frequency.Implicits

object Implicits extends Implicits
