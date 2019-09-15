package com.github.dlomsak.regex.deriv

/**
  * A deterministic finite automaton with abstract labeling. Initially, DFA states are labeled with regexes, but we
  * later resolve them to integers for efficiency.
  */
class DFA[A] private (states: Set[A], init: A, accepting: Set[A], slowDelta: Map[A, List[(CharClassAST, A)]], delta: Map[A, Map[Char, A]]) {
  final def accepts(s: String):Boolean = {
    val finalState = s.foldLeft(init)(delta(_)(_))
    accepting.contains(finalState)
  }

  // Useful for stepping through character consumption
  final def consume(c: Char): DFA[A] = new DFA(states, delta(init)(c), accepting, slowDelta, delta)


  // Generates a visualizable description of the DFA; to be fed in to GraphViz/Dot
  final def toDot: String = {
    val body = slowDelta.map { case (state,transitions) =>
      transitions.map { case (charCls, toState) =>
        val prefix = if (charCls.inverted) "~" else ""
        val transChars = charCls.chars.mkString(",")
        val label = if (charCls.chars.isEmpty && charCls.inverted)
          "\u03A3"
        else if (charCls.chars.size > 1)
          s"$prefix{$transChars}"
        else
          s"$prefix$transChars"
        s"""$state -> $toState [ label = \"$label\" ];"""
      }  mkString "\n"
    } mkString "\n"
    s"""digraph dfa {
        |node [color = white] "";
        |node [shape = doublecircle, color = black]; ${accepting.mkString(" ")};
        |node [shape = circle];
        |"" -> $init;
        |$body
        |}
     """.stripMargin
  }
}

object DFA {
  def apply[A](states: Set[A], init: A, accepting: Set[A], delta: Map[A, List[(CharClassAST, A)]]): DFA[A] = {
    /* transform {state -> [(charClass, nextState)]} into {state -> {char -> nextState}} for efficient transitioning
       * We know that for any given state, the character classes for transitions are disjoint or else we'd not have a DFA.
       * This means we can combine the character classes into a Map so that we can find the transition-to state in O(1) for
       * each input character. Because the classes can only be infinite by being the inverse of a finite set, we know that
       * one state will not have multiple infinite classes or else they would not be disjoint, and therefore any inverted
       * set serves as a default case if there is no specific transition in the Map. Because this is a DFA, delta must be
       * a total function and thus at least one class must be infinite. Therefore, there is exactly one infinite class to
       * transition on which will serve as the default case when none of the explicit transitions are matched.
       */
    val quickDelta: Map[A, Map[Char, A]] = delta map { case (inState, slowDelta) =>
      val (defaultStates, explicitTrans) = slowDelta partition { case (cc, outState) => cc.inverted }
      val defaultState = defaultStates.head._2 // always exactly 1 value from above reasoning
      val flatTrans = explicitTrans flatMap { case (cc, outState) =>
        cc.chars.map(c => c -> outState)
      }
      inState -> flatTrans.toMap.withDefaultValue(defaultState)
    }
    new DFA(states, init, accepting, delta, quickDelta)
  }
}
