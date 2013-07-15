Battleship: Generalized Binary Searh
=====

Here is our memory, mostly. Each item is a bounds.

    (3,5):
      expectedYield: 7
      centerFunctionValue: 8
    (8,10):
      expectedYield: 6
      centerFunctionValue: Nothing

When we run another iteration, we

* remove bounds that have a centerFunction (not Nothing).
* sort bounds by expectedYield.
* choose the one with the highest expectedYield as the "current" bounds.
* we run another iteration, passing the all of the bounds except the current one.

(And all iterations will generate new bounds.)
