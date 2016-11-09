package hoverboard.rewrite

import hoverboard.term.{Logic, Term}

trait Simplifier {
  def run(env: Env, term: Term): Term

  def run(term: Term): Term =
    // Let's freshen up, to stop multiple occurrences of the same function in a term interfere with simplification
    run(Env.empty, term.freshenIndices)

  def proveLeq(env: Env, leftTerm: Term, rightTerm: Term): Boolean =
    run(env, leftTerm =< rightTerm) == Logic.Truth
}

object Simplifier {
  def supercompiler: Simplifier = {
    lazy val scc: Simplifier = supercompiler(scc)
    scc
  }

  def supercompiler(internalSimplifier: => Simplifier): Simplifier =
    new Supercompiler(new Rippler(new Critiquer(internalSimplifier)), new Prover(internalSimplifier))

  def reducer: Simplifier = new Simplifier {
    override def run(env: Env, term: Term): Term =
      term.reduce(env)
  }
}
