# Markov Normal Algorithms

Markov Normal Algorithms provide a computational model which is as powerful as
Turing machine/lambda calculus. 

A classical Markov normal algorithm describes a transformation of strings into
strings. It is specified as a list of rewriting rules. The order of rules is
important. Each rule is given by a pair of strings
```
/match/substitute/
```
Each rule is either terminal or non-terminal. 

Operational semantics of the algorithm is provided by 

1. Initial current string `s` is the input string. 

1. If there is no rule for which `match` is a substring of `s`, then output `s`
   and terminate.

1. Otherwise, find the first rule `/match/substitute/` for which `match` is a substring of the current string
   `s`. 

1. Replace the first occurrence of `match` in `s` with `substitute`. 

1. If the rule is terminal, then output resulting string and terminate. 

1. Otherwise, resume from the beginning with the resulting string as a current string. 

This package provides a framework to construct Markov Normal Algorithms in
Haskell. 






