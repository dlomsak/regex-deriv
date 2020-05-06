package com.github.dlomsak.regex.deriv

/**
  * A deterministic finite automaton with abstract labeling. Initially, DFA states are labeled with regexes, but we
  * later resolve them to integers for efficiency.
  */
class DFA[A] private (states: Set[A], init: A, accepting: Set[A], slowDelta: Map[A, List[(CharClassAST, A, List[MatchAction])]], delta: Map[A, Map[Char, (A, List[MatchAction])]], env: Map[Int, String] = Map.empty) {
  final def accepts(s: String):Boolean = {
    val finalState = s.foldLeft(init)(delta(_)(_)._1)
    accepting.contains(finalState)
  }

  final def getEnv = env

  final def consumeAll(s: String): DFA[A] = s.foldLeft(this)((tdfa, c) => tdfa.consume(c))

  // Useful for stepping through character consumption
  final def consume(c: Char): DFA[A] = {
    val (nState, acts) = delta(init)(c)
    val env2 = acts.toSet.toList.foldLeft(env)((env, m) => env + (m.asInstanceOf[Emit].group -> (env.getOrElse(m.asInstanceOf[Emit].group, "") + c.toString)))
    new DFA(states, nState, accepting, slowDelta, delta, env2)
  }


  // Generates a visualizable description of the DFA; to be fed in to GraphViz/Dot
  final def toDot: String = {
    val body = slowDelta.map { case (state,transitions) =>
      transitions.map { case (charCls, toState, acts) =>
        val prefix = if (charCls.inverted) "~" else ""
        val transChars = charCls.chars.mkString(",")
        val symLabel = if (charCls.chars.isEmpty && charCls.inverted)
          "\u03A3"
        else if (charCls.chars.size > 1)
          s"$prefix{$transChars}"
        else
          s"$prefix$transChars"
        val label = s"$symLabel | $acts"
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

// TODO: recognize that this has turned into a TDFA
object DFA {
  def apply[A](states: Set[A], init: A, accepting: Set[A], delta: Map[A, List[(CharClassAST, A, List[MatchAction])]]): DFA[A] = {
    /* transform {state -> [(charClass, nextState)]} into {state -> {char -> nextState}} for efficient transitioning
       * We know that for any given state, the character classes for transitions are disjoint or else we'd not have a DFA.
       * This means we can combine the character classes into a Map so that we can find the transition-to state in O(1) for
       * each input character. Because the classes can only be infinite by being the inverse of a finite set, we know that
       * one state will not have multiple infinite classes or else they would not be disjoint, and therefore any inverted
       * set serves as a default case if there is no specific transition in the Map. Because this is a DFA, delta must be
       * a total function and thus at least one class must be infinite. Therefore, there is exactly one infinite class to
       * transition on which will serve as the default case when none of the explicit transitions are matched.
       */
    val quickDelta: Map[A, Map[Char, (A, List[MatchAction])]] = delta map { case (inState, slowDelta) =>
      val (defaultStates, explicitTrans) = slowDelta partition { case (cc, outState, acts) => cc.inverted }
      val defaultState = (defaultStates.head._2, defaultStates.head._3) // defaultStates always has exactly 1 value from above reasoning
      val flatTrans = explicitTrans flatMap { case (cc, outState, acts) =>
        cc.chars.map(c => c -> (outState, acts))
      }
      inState -> flatTrans.toMap.withDefaultValue(defaultState)
    }
    new DFA(states, init, accepting, delta, quickDelta)
  }
}
