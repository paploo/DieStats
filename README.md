# DieStats
(c) 2016, Jeff Reinecke

## Overview

This library will focus on producing several main pieces:

1. Structures for building and modeling die expressions, and
2. A net.paploo.diestats.statistics package focused around probability distribution functions.

## History

* v0.3.x (Sprint 2017) - I have even better ideas!
* v0.2.x (Spring 2016) - Started on a rewrite from scratch to be better.
* v0.1.x (Dec 2013) - An initial prototype to learn some Scala.

## Design Specification:

### Distributions

1. Formally define Probability values as a type wrapper.
2. Formally define Domains using a typeclass.
3. Introduce a type hierarchy for frequencies, PDFs and CDFs:
   - FrequencyDistribution: Defines counts against a domain A.
   - PDF: Defines probability distribution from frequencies.
   - CDF: Defines cumulative distribution from frequencies.

Some notes:

* Domain type classes really only need concat defined for convolution.
* Domains are sometimes ordered and sometimes not; distributions shouldn't care,
  instead we should have ways of getting the domain and ordering is applied later.
  (One way is to have the domain typeclass know about ordering, and have it have
   an "identity" ordering by default, which does nothing.)
* Need frequency buffers which allow fast, mutable accumulation.