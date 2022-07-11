package com.github.dlomsak.regex.deriv.phase

import com.github.dlomsak.regex.deriv.{CharClassAST, DFA, RegexAST}

import scala.annotation.tailrec

/**
  * Creates a DFA from a regular expression AST
  */

object RE2DFA {
  type State = RegexAST
  type Delta = Map[(State, CharClassAST), State]
  type States = Set[State]

  // used in explore to represent suspended work as values in place of recursive calls
  sealed trait ExploreStep
  final case class ExploreState(state: State) extends ExploreStep
  final case class ExploreTransitions(state: State, ccls: CharClassAST) extends ExploreStep

  /**
   * Find the set of states and delta function from an initial state via repeated derivation
   * This method is based on the goto/explore functions which are mutually recursive in the original paper, but
   * this implementation is tail recursive for the sake of the call stack.
   *
   * @param states the set of states created so far
   * @param delta the transitions between known states
   * @param work the list of pending states/transitions to search
   * @return all generated states and their transitions
   */
  @tailrec
  private def explore(states: States, delta: Delta, work: List[ExploreStep]): (States, Delta) = {
    work match {
      case Nil => (states, delta)
      case ExploreState(state) :: ws => explore(states, delta, state.getCharClasses.toList.foldLeft(ws)((wsAccum, ccls) => ExploreTransitions(state, ccls) :: wsAccum))
      case ExploreTransitions(state, ccls) :: ws =>
        // if the character class is empty, no transitions to make
        if (ccls.chars.isEmpty && !ccls.inverted) {
          explore(states, delta, ws)
        } else {
          // Find a character for derivation. All chars in the class will be treated the same so which one we take doesn't
          // matter. In the case of Sigma, just use 'a' as an arbitrary symbol as there is no point in bringing randomness
          // into the computation
          val c = if (ccls.chars.isEmpty && ccls.inverted) {
            'a'
          } else if (ccls.chars.nonEmpty && !ccls.inverted) {
            ccls.chars.head
          } else {
            // nonempty, inverted. Find a character in the class whose successor is not in it
            ccls.chars.map(_.toInt + 1).map(_.toChar).find(!ccls.chars.contains(_)).get
          }
          val qc = state.derive(c)
          val qcExist = states.find(_ == qc)
          // if derived state exists, add transition and move on with remaining work, otherwise explore derived state
          if (qcExist.nonEmpty) {
            explore(states, delta + ((state, ccls) -> qcExist.get), ws)
          } else {
            explore(states + qc, delta + ((state, ccls) -> qc), ExploreState(qc) :: ws)
          }
        }
    }
  }

  private def mkDFA(r: RegexAST): DFA[Int] = {
    val (states, delta) = explore(Set(r), Map.empty, List(ExploreState(r)))
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
