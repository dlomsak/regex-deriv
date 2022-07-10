[![CircleCI](https://circleci.com/gh/dlomsak/regex-deriv.svg?style=shield)](https://circleci.com/gh/dlomsak/regex-deriv)[![CircleCI](https://dl.circleci.com/status-badge/img/gh/dlomsak/regex-deriv/tree/master.svg?style=svg)](https://dl.circleci.com/status-badge/redirect/gh/dlomsak/regex-deriv/tree/master)

Regex-Deriv
==============
A Scala regular expression implementation with derivative-based evaluation described in the paper [Regular-expression derivatives re-examined](http://people.cs.uchicago.edu/~jhr/papers/2009/jfp-re-derivatives.pdf) by Scott Owens, John Reppy, and Aaron Turon.

Normalizing constructor approach inspired from [this Scheme implementation](https://github.com/tmmcguire/scheme-regular-expression-derivatives/blob/master/dre.scm)

<!--- Sub-expression matching takes ideas from [Regular Expression Sub-matching using Partial Derivatives](http://www.home.hs-karlsruhe.de/~suma0002/publications/ppdp12-part-deriv-sub-match.pdf) by Martin Sulzmann and Kenny Zhuo Ming Lu-->

Goals
-----
The main goal is to provide a regex library that supports regular operations that are usually absent (e.g., intersection) and avoids backtracking and extra-regular features (e.g., back matching) to avoid exponential blowups. While the Thompson caching NFA construction is a well-known way to achieve this, the state space can get large and it is not necessary to construct an automaton to perform matching using derivatives.

Features
-------
* Character classes:
    * [a-zA-Z] syntax for ranges
    * [^abc] for negation
    * \d\D\w\W\s\Sv\V\h\H escape sequences for common classes
* Bounded repetition:
  * r{n}
  * r{i,}
  * r{i,j}
* Intersection (&) and negation (~) supported in regular expressions
* Compilation of regex string to an AST
* Direct matching of strings against ASTs
  * derive(c: Char) can be used to rewrite the AST one input character at a time
* DFA construction from regular expression
  * Matching of input strings against DFA
  * GraphViz Dot representation of DFA which can be rendered with appropriate tooling

