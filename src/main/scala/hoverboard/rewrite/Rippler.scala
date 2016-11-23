package hoverboard.rewrite

import hoverboard.term._

import scalaz.Scalaz._
import scalaz.{Name => _, _}

/**
  * Implements rippling, using a provided critiquing engine if rippling gets blocked
  */
class Rippler(critiquer: Critiquer) {
  private def mergeDivingRipples(ripples: IList[Ripple], originalGoal: Term): Ripple = {
    val generalisation = Substitution.unionDisjoint(ripples.map(_.generalisation))
    val skeletons = ripples.flatMap(_.skeletons)
    val goal = originalGoal.withImmediateSubterms(ripples.map(_.goal))
    Ripple(skeletons, goal, generalisation)
  }

  private def mergeCouplingRipples(ripples: IList[Ripple], skelFun: Term, goalFun: Term): Ripple = {
    val newSkeletons: IList[Term] = ripples
      .map(_.skeletons)
      .sequence[({ type G[X] = IList[X] })#G, Term]
      .map { args => skelFun.apply(args) }
    val newGoal = goalFun.apply(ripples.map(_.goal))
    val newGen = Substitution.unionDisjoint(ripples.map(_.generalisation))
    Ripple(newSkeletons, newGoal, newGen)
  }

  private def dive(env: Env, skeleton: Term, goal: Term): Ripple = {
    val subtermRipples = goal.immediateSubterms.map(run(env, skeleton, _))
    mergeDivingRipples(subtermRipples, goal)
  }

  def run(skeleton: Term, goal: Term): Ripple =
    run(Env.empty, skeleton, goal)

  def run(env: Env, skeleton: Term, goal: Term): Ripple =
    (skeleton, goal) match {
      case (skeleton: Var, goal: Var) =>
        Ripple(IList(skeleton), skeleton, goal / skeleton.name)

      case (AppView(skelCon: Constructor, skelArgs: IList[Term]),
      AppView(goalCon: Constructor, goalArgs: IList[Term]))
        if skelCon == goalCon && skelArgs.length == goalArgs.length =>
        val ripples = skelArgs.fzipWith(goalArgs)(run(env, _, _))
        mergeCouplingRipples(ripples, skelCon, goalCon)

      case (CriticalPair(skelFix: Fix, skelArgs: IList[Term], skelCp),
            CriticalPair(goalFix: Fix, goalArgs: IList[Term], goalCp))
          // Critical-path aware coupling
          if skelArgs.length == goalArgs.length && skelCp.couplesWith(goalCp) =>

        val argRipples = skelArgs.fzipWith(goalArgs)(run(env, _, _))
        val mergedRipple = mergeCouplingRipples(argRipples, skelFix, goalFix)
        critiquer.run(env, mergedRipple)

      case _ =>
        dive(env, skeleton, goal)
    }
}
