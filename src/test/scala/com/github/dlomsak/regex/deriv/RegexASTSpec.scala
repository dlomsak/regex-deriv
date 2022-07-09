package com.github.dlomsak.regex.deriv


class RegexASTSpec extends BaseSpec {

  "CatAST" should "associate to the right" in {
    forAll { (r1: RegexAST, r2: RegexAST, r3: RegexAST) =>
      CatAST(CatAST(r1, r2), r3) should equal (CatAST(r1, CatAST(r2, r3)))
    }
  }

  it should "not generally commute over equality" in {
    forAll { (r1: RegexAST, r2: RegexAST) =>
      whenever(!r1.acceptsEmpty && !r1.isNull && !r2.acceptsEmpty && !r2.isNull && r1 != r2) {
        CatAST(r1, r2) should not equal CatAST(r2, r1)
      }
    }
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
    RegExpr("\\d").right.get.accepts("5") shouldBe true
    RegExpr("\\D").right.get.accepts("5") shouldBe false
    RegExpr("\\w").right.get.accepts("b") shouldBe true
    RegExpr("\\W").right.get.accepts("b") shouldBe false
    RegExpr("\\s").right.get.accepts(" ") shouldBe true
    RegExpr("\\S").right.get.accepts(" ") shouldBe false
    RegExpr("\\v").right.get.accepts("\r") shouldBe true
    RegExpr("\\V").right.get.accepts("\r") shouldBe false
    RegExpr("\\h").right.get.accepts("\t") shouldBe true
    RegExpr("\\H").right.get.accepts("\t") shouldBe false
  }

}
