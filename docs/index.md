This is a passion project that is the result of the practical uses I've had for regular expressions over the years, the downsides with PCRE-based implementations (which are often the standard), and my admiration of the elegance of regex derivatives. I intend to make the implementation performant and document the challenges I have run into in doing so to serve as the sort of guide I wish I had at the start.

### Basic Usage

The `apply` method of the `RegExpr` object is the main entry point to the library, which tokenizes and parses a given regex string into an AST representing that expression.

```scala
// compile a regex, returns Either[RegexCompilationError, RegexAST]
val ast = RegExpr("[a-zA-Z0-9]+").right.get

ast.matches("a") // returns true
ast.matches("alpha123") // returns true
ast.matches("") // returns false
ast.matches(";") // returns false
```

ASTs can also be applied to strings to compute the derivative expression from that character sequence. Any time that `RegexAST.acceptsEmpty` returns `true`, the derived sequence of input characters up to that point represents a string that is accepted by the regular expression.

```scala
ast.acceptsEmpty // returns false because our original expression accepts length > 0 strings due to + operator
ast("a").acceptsEmpty // returns true
ast("alpha123").acceptsEmpty // returns true
```

When we have consumed a series of characters that take us out of the language (that is, no strings with this prefix can match the orignal expression), the result is NullAST representing the null language.

```scala
ast(";") // returns NullAST
```

### String Generation

The DFA has a `getStrings` method that returns a `Stream[String]` containing values that are in the language. The stream will be endless if
the language is infinite, but the strings are emitted in shortest-path order. Not every string will be emitted: wildcard characters will
simply generate a single default character, and inverted character classes (e.g., [^abc]) will generate a single character outside the
given characters (e.g., 'd'). Any finite class of characters will be enumerated in full.

What is powerful about intersection, complementation, and generation together is that you can generate strings adhering to a 
number of constraints by describing each constraint individually and generating strings from their combination. For example:

```scala
val emailTemplate = "[a-zA-Z0-9]+@.+"
val nonNumberStart = "(~(\\d.*))" // the strings that don't begin with a digit, including the empty string
val emailDomains = ".*@[a-zA-Z]+\\.(com|net|[a-zA-Z][a-zA-Z0-9]\\.co\\.uk|[a-z]+\\.edu)"
val emailLength = ".{10}"
val singleAt = "[^@]*@[^@]*"
val emailFull = s"$emailTemplate&$nonNumberStart&$emailDomains&$emailLength&$singleAt"
val dfa = RE2DFA(RegExpr(emailFull).right.get)
val testOutputs = dfa.getStrings.take(1000)
dfa.accepts("a@test.com") // true
dfa.accepts("ab@test.com") // false; > 10 characters
testOutputs.forall(_.length == 10) // true
testOutputs.forall(_.count(_=='@') == 1) // true
testOutputs.exists(_.head.isDigit) // false
testOutputs.forall(s => s.endsWith(".com") || s.endsWith(".net") || s.endsWith(".co.uk") || s.endsWith(".edu")) // true
```
The first string emitted by the above is `A@AA.n.edu` which has the intended structure.