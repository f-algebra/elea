package elea.rewrite

import elea.term._

import scalaz.NonEmptyList

class Prover(simplifier: => Simplifier) {

  def assertionFromMatch(matchedTerm: Term, pattern: Pattern): Context =
    C { gap =>
      val branches = NonEmptyList[Branch](PatternBranch(pattern, Var(gap)), DefaultBranch(Bot))
      Case(matchedTerm, branches, Case.Index.fresh)
    }

  /**
    * Check if the set of pattern matches in this environment is unsatisfiable,
    * aka this branch is unreachable
    */
  def unsatisfiable(env: Env): Boolean =
    if (env.matches.size < 2) {
      false
    } else {
      val matchConjunction = env.matches.toSeq
        .map((assertionFromMatch _).tupled)
        .reduce[Context] { case (ctx1, ctx2) => ctx1.composeWith(ctx2) }
      val result = matchConjunction
          .composeWith(matchConjunction)
          .apply(Logic.Falsity)
          .reduce
      result == Logic.Truth
    }
}
