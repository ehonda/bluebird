# About

This is an implementation of a string rewrite system that is based on an algorithm introduced by Ikebuchi and Nakano in <https://arxiv.org/abs/1901.11010>. It operates on lists of natural numbers and produced weakly decreasing lists of natural numbers. For a detailed description with an example of how the algorithm operates see Example 3.2 in the referenced paper. From this algorithm, we obtain the reductions of a starting word w by fixing a word c to append and repeatedly applying the algorithm with L_2 = c, obtaining the sequence:

[w, A(w, c), A(A(w, c), c), ...]

where A(w, c) be the output of the algorithm with L_1 = w and L_2 = c. We write w ->c v if v = A(w, c).

The implementation was written as part of a project where I explored how the sequences of derivates of the string rewrite system behave. Usability and performance weren't the top concerns as I was writing the code while trying to understand the system, so there are a lot of places where it could be improved.

# Usage Examples

Load module Main in GHCI. We can look at the reductions w ->c v ->c u ->c ...:

```
*Main> peek 0 10 $ reductions [2] [0, 0]
[0,0]
[4]
[3,2]
[4,2,1]
[5,3,1,0]
[6,4,2,0]
[5,5,3,1]
[4,4,4,2,0]
[7,3,3,3,1]
[6,6,2,2,2,0]
```

Here we have c = [2] and w = [0, 0]. Further, the function peek is a utility function to easily inspect a part of an infinite list:

```haskell
peek start n xs = mapM_ print $ drop start $ take (start + n) xs
```

An algorithm to find cycles in an list of the form [x, f(x), f(f(x)), ...], known as [Floyd's Algorithm](https://en.wikipedia.org/wiki/Cycle_detection#Floyd's_Tortoise_and_Hare) is also implemented:

```
*Main> findCycle $ reductions [2] [0, 0]
(116,36)
```

This means there is a cycle starting at index 116 of length 36. We are also interested in the behaviour for different starting words. To that end, a few functions are implemented that give us infinite lists of weakly decreasing starting words. Have a look at the source of module Bluebird where they are listed. Here is one example of a list of starting words which includes all possible words:

```
*Main> peek 0 10 $ w0_dove
[]
[0]
[0,0]
[1]
[2]
[1,0]
[0,0,0]
[0,0,0,0]
[1,0,0]
[1,1]
*Main> peek 1000 10 $ w0_dove
[2,2,2,1,1,1,1,1,0,0]
[2,2,2,1,0,0,0,0,0,0,0]
[2,2,1,1,1,1,1,1,1,1,0,0]
[2,2,1,1,1,1,1,0,0,0,0,0,0]
[2,2,1,1,0,0,0,0,0,0,0,0,0,0]
[2,1,1,1,1,1,1,1,1,1,1,1,1,1,1]
[2,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0]
[2,1,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0]
[2,1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,0,0]
[2,1,1,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0]
```

Hint: Try out the commands `display coloredBoxes_0 1 10000 w0_dove` and `display coloredNumbers_0 1 10000 w0_dove`.

With these starting word lists, we can acquire data about cycles found for these starting words.

```
*Main> peek 0 10 $ cycles [0] w0_dove
(6,4)
(5,4)
(3,4)
(4,4)
(2,4)
(1,4)
(0,4)
(2,3)
(0,3)
(0,4)
*Main> mapM_ print $ uniqueCycles 500 [0] w0_dove
(10,4,[[0,0,0],[3],[2,0],[1,1]])
(256,3,[[1,0,0],[3,0],[2,1]])
(124,3,[[1,1,0],[3,0,0],[2,2]])
(110,2,[[2,0,0],[3,1]])
```

The function cycles maps `findCycle` to the list of reduction sequences, `uniqueCycles` lists the unique cycles in the form (count, length, cycle) for the first n reduction sequences.

There are various ways to visualize a sequence of reductions. Here is an example

![Screenshot of the visualization](img/display_boxes.png)

For more examples, please refer to the various demo functions implemented in module Main.
