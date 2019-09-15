package com.github.dlomsak.regex.deriv.phase

import com.github.dlomsak.regex.deriv.BaseSpec


class RELexerSpec extends BaseSpec {
  "RELexer" should "succeed but generate no tokens on the empty input" in {
    RELexer("") shouldBe Right(Seq())
  }

  it should "generate all of the defined tokens" in {

    RELexer("""|\a1~?[(+&])*-^{,}""") shouldBe Right(Seq(
      PIPE,
      BACKSLASH,
      CHARLIT('a'),
      DIGITLIT('1'),
      TILDE,
      HOOK,
      LBRACKET,
      LPAREN,
      PLUS,
      AMPER,
      RBRACKET,
      RPAREN,
      STAR,
      DASH,
      CARET,
      LBRACE,
      COMMA,
      RBRACE))
  }
}
