package com.github.dlomsak.regex.deriv

// test specific matching behaviors
class MatchSpec extends BaseSpec {
  "complementation" should "match correctly" in {
    val ast = RegExpr("~(a*)").right.get
    ast.matches("") shouldBe false // the empty string is in a*
    ast.matches("a") shouldBe false
    ast.matches("b") shouldBe true
    ast.matches("bbbbbbb") shouldBe true
    ast.matches("bbbbbabb") shouldBe true
    ast.matches(";.@") shouldBe true
  }

  "intersection" should "match correctly" in {
    // this expr doesn't match anything
    val ast = RegExpr("(a*b)&(b*a)").right.get
    ast.matches("") shouldBe false
    ast.matches("a") shouldBe false
    ast.matches("b") shouldBe false
    ast.matches("ab") shouldBe false
    ast.matches("ba") shouldBe false

    // matches 3-digit numbers
    val ast2 = RegExpr("[a-zA-Z0-9]{3}&(\\d)*").right.get
    ast2.matches("") shouldBe false
    ast2.matches("aaa") shouldBe false
    ast2.matches("1") shouldBe false
    ast2.matches("11") shouldBe false
    ast2.matches("111") shouldBe true
    ast2.matches("1111") shouldBe false
  }
}
