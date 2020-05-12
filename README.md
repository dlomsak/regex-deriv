[![CircleCI](https://circleci.com/gh/dlomsak/regex-deriv.svg?style=shield)](https://circleci.com/gh/dlomsak/regex-deriv)

Regex-Deriv
==============
A Scala regular expression implementation with derivative-based evaluation described in the paper [Regular-expression derivatives re-examined](http://people.cs.uchicago.edu/~jhr/papers/2009/jfp-re-derivatives.pdf) by Scott Owens, John Reppy, and Aaron Turon.

Normalizing constructor approach inspired from [this Scheme implementation](https://github.com/tmmcguire/scheme-regular-expression-derivatives/blob/master/dre.scm)

Sub-expression matching takes ideas from [Regular Expression Sub-matching using Partial Derivatives](http://www.home.hs-karlsruhe.de/~suma0002/publications/ppdp12-part-deriv-sub-match.pdf) by Martin Sulzmann and Kenny Zhuo Ming Lu

Goals
-----
The main goal is to provide a regex library that avoids backtracking and extra-regular features (e.g., back matching) to achieve stable performance without pathological cases. While the Thompson caching NFA construction is a well-known way to achieve this, the state space can get large. Finally, the lexer/parser should be extensible to support additional syntactic elements that expand into standard regular expressions.

Roadmap
-------
* ~~Lexer and parser for basic regex operators~~
* ~~Implement regex derivatives for all AST structures and method for performing string matching~~
* ~~add tests and use property-based testing~~
* ~~CI with code coverage~~
* ~~support character classes~~
* ~~implement automaton construction, move away from matching via AST re-writing~~
* ~~support common character class escape sequences such as \d for digits~~
* ~~support quantification (e.g., "r{n, m}" matches n to m occurrences of r)~~
* reimplement parser to remove backtracking
* support group extraction
* support named groups
* support directives such as ?i for case insensitivity
* performance testing against scala.util.matching.regex
