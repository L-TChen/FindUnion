# Find-Union 

## An imperative implementation of disjoint-set data structure in Haskell

This implementation is based on the original [Find-Union
algorithm](https://dl.acm.org/citation.cfm?id=321884) by Tarjan. It provides
the standard set of imperative operations in `Data.Union.ST`, i.e. `create`,
`union`, and `find` (also `connected`), and the type of elements can be any
instance of the type class
[`Ix`](https://www.haskell.org/onlinereport/haskell2010/haskellch19.html).
The worst-case time complexity of `union` and `find` in `Data.Union.ST` is
*O(α(n))* where *α(n)* is the inverse Ackermann function. See
[this](https://en.wikipedia.org/wiki/Disjoint-set_data_structure) Wikipedia
page for details.

In addition, it provides a pure interface in `Data.Union` with a creation operation

    buildDS :: (Ix i) => Bound i -> [(i, i)] -> Union i [i]

which takes the range of elements and a list of pairs of the same set and
produces a table to check if any two elements are of the same set using
`connected`. This operation constructs the table within the ST monad and
*rebalance* every element so that its time complexity is *O(n * α(n))*,
practically linear, where *n* is the length of range and the time complexity of
`find` and `connected` is only *O(1)*. Unfortunately, I did not implement a
`union` operation for this pure interface, as any update to a persistent array
takes *O(n)* steps in Haskell. 

Alternatively, you can also use 

    fromList :: (Ix i) => (i, i) -> [[i]] -> Union i [i]

to construct a table to look up which takes, again, the range of elements and a
list of lists (instead of pairs) of the same set. Any missing element is
regarded as a singleton set. 

To the best of my understanding, there is currently no
functional/persistent solution to this problem achieving the same time
complexity. If you happen to know how to do it, please let me know. 

This package is *not* going to Hackage for now.
