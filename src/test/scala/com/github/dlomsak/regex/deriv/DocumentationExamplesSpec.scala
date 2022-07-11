package com.github.dlomsak.regex.deriv

import com.github.dlomsak.regex.deriv.phase.{ParseContext, RE2DFA}

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

  it should "generate all emails" in {
    val emailTemplate = "[a-zA-Z][a-zA-Z0-9]*@.+"
    val emailDomains = ".*@[a-zA-Z]+\\.(com|net|[a-zA-Z][a-zA-Z0-9]\\.co\\.uk|[a-z]+\\.edu)"
    val emailLength = ".{10}"
    val dfa = RE2DFA(RegExpr(s"$emailTemplate&$emailDomains&$emailLength").right.get, new ParseContext)
    val testOutputs = dfa.getStrings.take(1000)
    dfa.accepts("a@test.com") shouldBe true
    dfa.accepts("ab@test.com") shouldBe false // too long
    testOutputs.forall(_.length == 10) shouldBe true
    testOutputs.forall(_.contains("@")) shouldBe true
    testOutputs.forall(s => s.endsWith(".com") || s.endsWith(".net") || s.endsWith(".co.uk") || s.endsWith(".edu")) shouldBe true
  }
}