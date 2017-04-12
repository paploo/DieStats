package net.paploo.diestats.test

import org.scalatest.{BeforeAndAfterEach, Matchers, FunSpec}

trait SpecTest extends FunSpec with Matchers with BeforeAndAfterEach {

  // The acceptable error to use in double floating point precision numerical "equality" tests.
  val ε = 1e-12

}
