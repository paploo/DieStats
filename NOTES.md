# Notes

## Die Expressions

Die Expressions have different subsets of legal operations depending on the
rules about the domain.

A few notes:
- "summing" two dice is really convlution, e.g. `d6 + d6` is `d6 convolve d6`.
- `3d6` is repeated convolution, and is thus `d6 + d6 + d6` in this example.
- A constant is merely a one-sided die, e.g. `d6 + 7` is `d6 + d{7}`.
- Don't confuse multiplication with convolution, e.g. `2 x d6` is not `2d6`, rather
  `2 x d6` evalutates a single `d6` and then multiplies by 2.
- It is possible to have `d6 x d6` which multiplies two die rolls, thus `2 x d6`
  is really just `d{2} x d6`
- It would be possible to have to convolve a d6, d3 number of times, which
  we could do as `(d3)d6` or simply `d3d6`

## Example Problems

#### Coin Flip Series

Problem: In n coin flips, find the probability of m tails in a row.

As an example, start with 3 coins, so that we get the possible outcomes:
`TTT`, `THT`, `HTT`, `HHT`, `TTH`, `THH`, `HTH`, `HHH`.

This is outside of the realm of just a die expression, but could be determined by
convolving `3d{T,H}` with either a String Monoid domain (since each side has a
single character label), or with a standard `Seq[String]` Monoid.

The expression could be evaluated to produce a `ProbabilityDistribution`, and
a `filter` could be done on the pairs to find only those with the desired count of
`T`'s in a row.

#### Coin Flip War Game

Define a simple war game where for each soldier in your army, you flip a coin,
and for each heads you remove an enemy soldier.

The dice domain is "H" and "T", and is technically unsorted.

For an army of size n, the die expression is `nd{T,H}`.

For a simple 3 man army, the raw flip outcomes are
`TTT`, `THT`, `HTT`, `HHT`, `TTH`, `THH`, `HTH`, `HHH`;
however we are interested in the *counts*, and thus `THT`, `TTH`, and`HTT`, which
each have 2 tails and one head, are considered the same.

We then combine ones where the counts are the same (binomial distribution
with p=1/2):
`TTT -> 1/8, TTH -> 3/8, THH -> 3/8, HHH -> 1/8`

If the domain is expressed as a `Seq[String]` (where `"H"` and `"T"` are the string
label), and during convolution we supply a monad that predictably orders the domain
values, then it'd combine those with the same counts of T and H.

Alternatively, one could build the domain as a `Map[String, Long]`, where the key
is the label, and the value is the count (e.g. `THH` would be `Map("T" -> 1, "H" -> 2)`),
using a Monoid where concatenation adds any missing keys and increments the count by 1.

TODO: Implement the above Map Monad—this is more straight forward than the
`SeqSet` monoid that does the same job.

Alternatively, given the small, concrete length of hte domain, could build the
domain as a tuple of counts `(Long, Long)`.

##### Variation

As a variation, consider an example where you roll n+1 dice, and take the highest
n rolls, skewing the distribution in our favor.

To do this, we must consider that we have an ordering over the domain, where
`T` is less than `H`.

For a simple 2 man army, the raw flip outcomes are:
`TTT`, `THT`, `HTT`, `HHT`, `TTH`, `THH`, `HTH`, `HHH`.

We then take the 2 highest rolls in each of these:
`TT`, `TH`, `TH`, `HH`, `TH`, `HH`, `HH`, `HH`.

This produces the distribution:
`TT -> 1/8, TH -> 3/8, HH -> 4/8`

Technically above method convolves the domain with simple concatenation,
filters the values, and then reduces using the monoidal rules.
 
This is better implemented by convolving properly to begin with, filtering,
and sum:

The three flips produce:
`TTT -> 1/8, TTH -> 3/8, THH -> 3/8, HHH -> 1/8`

We then filter:
`TT -> 1/8, TH -> 3/8, HH -> 3/8, HH -> 1/8`

And then sum where the domains became equal:
`TT -> 1/8, TH -> 3/8, HH -> 41/8`

#### Classic Board Game Movement

#### Apocalypse System Probabilities

#### Open d20 Character Gen

To produce a stat, We roll 4d6, and take the highest 3.
In extended notations, this is sometimes `4d6b3`, where `b` is `best`, and
thus `w` would be `worst`.

To gain traction, let's first consider the smaller `3d3b2`, which has outcomes:
`1,1,1`, `1,1,2`, `1,1,3`, `1,2,1`, `1,2,2`, `1,2,3`, `1,3,1`, `1,3,2`, `1,3,3`,
`2,1,1`, `2,1,2`, `2,1,3`, `2,2,1`, `2,2,2`, `2,2,3`, `2,3,1`, `2,3,2`, `2,3,3`,
`3,1,1`, `3,1,2`, `3,1,3`, `3,2,1`, `3,2,2`, `3,2,3`, `3,3,1`, `3,3,2`, `3,3,3`.

Filtering the best 2, we get:
`1,1`, `1,2`, `1,3`, `1,2`, `2,2`, `2,3`, `1,3`, `3,2`, `3,3`,
`2,1`, `2,2`, `2,3`, `2,2`, `2,2`, `2,3`, `2,3`, `2,3`, `3,3`,
`3,1`, `3,2`, `3,3`, `3,2`, `3,2`, `3,3`, `3,3`, `3,3`, `3,3`.

We then normalize to:
`1,1 -> 1/27, 1,2 -> 2/27, 1,3 -> 2/27,
 2,1 -> 1/27, 2,2 -> 4/27, 2,3 -> 5/27,
 3,1 -> 1/27, 3,2 -> 4/27, 3,3 -> 7/27.`
 
Finally, we sum on the domain and get:
`2 -> 1/27, 3 -> 3/27, 4 -> 7/27, 5 -> 9/27, 6 -> 7/27`

Unlike the heads/tails example, the domain is numeric instead of a list, so
pre-convolution destroys the information necessary to determine bestness.

Thus, in general, determining best requires:
1. doing the convolution using a simple `Monoid` that concatenates the series of
   values on an array,
2. using an `Ordering` on the domain during a map operation that transforms
   the domain to only have the desired values.
3. Using a `Monoid` to reduce the domain to the desired form (this is the same
   monoid that would be used during normal convolution).
   
This sequence can be extended beyond a simple take best/worst, into a more generalized
map/reduce operation where a series of expressions are passed in. At that point,
even basic convolution is a subset of this operation, where the mapping phase
uses `identity` as the domain mapping function.