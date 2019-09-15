package com.github.dlomsak.regex.deriv

import com.github.dlomsak.regex.deriv.phase.RE2DFA

class DFASpec extends BaseSpec {
  "DFA" should "match equivalently to RegexAST" in {
    forAll { (r: RegexAST) =>
      val dfa = RE2DFA(r)
      forAll { (s: String) =>
        r.matches(s) shouldEqual dfa.accepts(s)
      }
    }
  }
}
