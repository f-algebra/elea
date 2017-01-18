package elea.rewrite

import elea.term._

trait Simplifier {
  def run(env: Env, term: Term): Term

  def run(term: Term): Term =
    // Let's freshen up, to stop multiple occurrences of the same function in a term interfere with simplification
    run(Env.empty, term.freshenIndices)

  def proveLeq(env: Env, leftTerm: Term, rightTerm: Term): Boolean =
    run(env, leftTerm =< rightTerm) == Logic.Truth
}

object Simplifier {
  def supercompilation: Simplifier = {
    lazy val scc: Simplifier = supercompilation(scc)
    scc
  }

  def supercompilation(internalSimplifier: => Simplifier): Simplifier =
    new Supercompiler(new Rippler(new Critiquer(internalSimplifier)), new Prover(reduction))

  def reduction: Simplifier = new Simplifier {
    override def run(env: Env, term: Term): Term =
      term.reduce(env)
  }
}
