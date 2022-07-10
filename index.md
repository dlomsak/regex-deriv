## Regex-deriv
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

ASTs can also be applied to strings to compute the derivative expression from that character sequence. Any time that `RegexAST.acceptsEmpty` returns ` true`, the derived sequence of input characters up to that point represents a string that is accepted by the regular expression.

```scala

ast.acceptsEmpty // returns false because our original expression accepts length > 0 strings due to + operator
ast("a").acceptsEmpty // returns true
ast("alpha123").acceptsEmpty // returns true
```

When we have consumed a series of characters that take us out of the language (that is, no strings with this prefix can match the orignal expression), the result is NullAST representing the null language.

```scala

ast(";") // returns NullAST
```

