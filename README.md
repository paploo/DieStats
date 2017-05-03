# DieStats
(c) 2016, Jeff Reinecke

Main Repo and issue tracker (login required):
https://gitlab.com/paploo/DieStats

Public Mirror (no login required):
https://github.com/paploo/DieStats

## Overview

This library provides mechanisms for doing statistical analysis on dice expressions;
it consists of four main parts:

1. A statistics library for working with distributions over discrete domains,
2. A dice expression AST for structuring expressions that can be evaluated in various ways,
3. A parser that converts string dice expressions into an AST, and
4. A command line utility that fronts the parser and produces result output.

## History

* v0.3.x (Spring 2017) - Complete rewrite to facilitate non-numerical domains.
* v0.2.x (Spring 2016) - Started on a rewrite from scratch to be better.
* v0.1.x (Dec 2013) - An initial prototype to learn some Scala.

## Task Overview

* (90%) Statistics Library Implementation
* (30%) Expression Library Implementation
* (_) Parser Library Implementation
* (_) CLI Implementation