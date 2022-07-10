package com.github.dlomsak.regex.deriv


class RegexASTSpec extends BaseSpec {

  "CatAST" should "combine when nested" in {
    forAll { (r1: RegexAST, r2: RegexAST, r3: RegexAST) =>
      CatAST(List(CatAST(List(r1, r2)), r3)) should equal (CatAST(List(r1, r2, r3)))
    }
  }

  it should "not generally commute over equality" in {
    forAll { (r1: RegexAST, r2: RegexAST) =>
      whenever(!r1.acceptsEmpty && !r1.isNull && !r2.acceptsEmpty && !r2.isNull && r1 != r2) {
        CatAST(List(r1, r2)) should not equal CatAST(List(r2, r1))
      }
    }
  }

  it should "derive successfully over long sequences" in {
    CatAST(List.fill(10000)(CharAST('a'))).apply("a"*10000).acceptsEmpty should equal (true)
  }

  "OrAST" should "associate to the right" in {
    forAll { (r1: RegexAST, r2: RegexAST, r3: RegexAST) =>
      whenever(r1 != r2) {
        OrAST(OrAST(r1, r2), r3) should equal (OrAST(r1, OrAST(r2, r3)))
      }
    }
  }

  it should "commute over equality" in {
    forAll { (r1: RegexAST, r2: RegexAST) =>
      OrAST(r1, r2) shouldEqual OrAST(r2, r1)
    }
  }

  "escaped character classes" should "match appropriate characters" in {
    RegExpr("\\d").right.get.matches("5") shouldBe true
    RegExpr("\\D").right.get.matches("5") shouldBe false
    RegExpr("\\w").right.get.matches("b") shouldBe true
    RegExpr("\\W").right.get.matches("b") shouldBe false
    RegExpr("\\s").right.get.matches(" ") shouldBe true
    RegExpr("\\S").right.get.matches(" ") shouldBe false
    RegExpr("\\v").right.get.matches("\r") shouldBe true
    RegExpr("\\V").right.get.matches("\r") shouldBe false
    RegExpr("\\h").right.get.matches("\t") shouldBe true
    RegExpr("\\H").right.get.matches("\t") shouldBe false
  }

}
