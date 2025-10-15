A collections of prototype implementations of fuzzy search across dictionary.

The aim was mostly to have comparison points in term of complexity and performance
for the OCaml module error messages implementation.

The various implementations tested:

- Simple quadratic method
- Symmetric deletion methods
- Burkhard-Keller tree method
- Trie and Levenstein-automaton method
- Naive trie based method

For the expected parameters of OCaml error messages, the simple quadratic method
is fast enough and in fact the fastest one method without further optimisation
on the limited benchmark used.

For larger size, symmetric deletions dictionaries seems like another good option in term
of complexity and performance (it starts to outperform the simple method with more than 500
elements to match).

The current implementation of trie-based methods is slower than the simple methods with less
than 3000 elements to pair. It could be optimized using Forward-backward pair for instance, but
at a cost of a certain increase in complexity.
