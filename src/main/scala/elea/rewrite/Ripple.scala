package elea.rewrite

import elea.term.{Substitution, Term}

import scalaz.IList

case class Ripple(
  skeletons: IList[Term],
  goal: Term,
  generalisation: Substitution) {

  def mapGoal(f: Term => Term) = copy(goal = f(goal))
}
