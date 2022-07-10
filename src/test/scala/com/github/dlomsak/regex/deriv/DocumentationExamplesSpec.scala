package com.github.dlomsak.regex.deriv

// adding code used in documentation to make sure the results agree and alert to regressions that affect documentation
class DocumentationExamplesSpec extends BaseSpec {
  val ast: Either[RegexCompilationError, RegexAST] = RegExpr("[a-zA-Z0-9]+")

  "AST example" should "compile successfully" in {
    ast shouldBe 'Right
  }

  it should "match the right strings" in {
    val astr = ast.right.get
    astr.matches("a") shouldBe true
    astr.matches("alpha123") shouldBe true
    astr.matches("") shouldBe false
    astr.matches(";") shouldBe false
  }

  it should "acceptsEmpty according to examples" in {
    val astr = ast.right.get
    astr.acceptsEmpty shouldBe false
    astr("a").acceptsEmpty shouldBe true
    astr("alpha123").acceptsEmpty shouldBe true
  }

  it should "give NullAST on a bad match" in {
    ast.right.get(";") shouldBe NullAST
  }
}