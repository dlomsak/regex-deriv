package com.github.dlomsak.regex.deriv.phase

import com.github.dlomsak.regex.deriv.{CharClassAST, DFA, RegexAST}

/**
  * Creates a DFA from a regular expression AST
  */

object RE2DFA {
  type State = RegexAST
  type Delta = Map[(State, CharClassAST), State]
  type States = Set[State]

  private def goto(state: State)(st: (States, Delta), s: CharClassAST): (States, Delta) = {
    val (states, delta) = st

    // if the character class is empty, no transitions to make
    if (s.chars.isEmpty && !s.inverted) return st

    // Find a character for derivation. All chars in the class will be treated the same so which one we take doesn't
    // matter. In the case of Sigma, just use 'a' as an arbitrary symbol as there is no point in bringing randomness
    // into the computation
    val c = if (s.chars.isEmpty && s.inverted) {
      'a'
    } else if (s.chars.nonEmpty && !s.inverted) {
      s.chars.head
    } else {
      // nonempty, inverted. Find a character in the class whose successor is not in it
      s.chars.map(_.toInt + 1).map(_.toChar).find(!s.chars.contains(_)).get
    }
    val qc = state.derive(c)
    states.find(_.equals(qc)).map { qPrime =>
      (states, delta + ((state, s) -> qc))
    } getOrElse {
      explore(states + qc, delta + ((state, s) -> qc), qc)
    }
  }

  private def explore(states: States, delta: Delta, state: State): (States, Delta) =
    state.getCharClasses.foldLeft((states, delta))(goto(state))

  private def mkDFA(r: RegexAST): DFA[Int] = {
    val (states, delta) = explore(Set(r), Map.empty, r)
    val accepting = states.filter(_.acceptsEmpty)
    // label states numerically rather than by regex
    val nStates = states.zipWithIndex.toMap
    val nStatesS = nStates.map { case (k, v) => (k.toString, v) }
    val nDelta = delta.map { case ((s1, cc), s2) => ((nStatesS(s1.toString), cc), nStatesS(s2.toString)) }
    // compute state -> (charClass, nextState) structure for DFA because we can't directly do Map lookups on char classes
    val nDeltaSt = nDelta.toList.map{case ((s1, cc), s2) => (s1, (cc, s2))}.groupBy(_._1).mapValues(_.map(_._2))
    DFA(nStates.values.toSet, nStates(r), accepting.map(nStates), nDeltaSt)
  }

  def apply(r: RegexAST): DFA[Int] = mkDFA(r)
}
