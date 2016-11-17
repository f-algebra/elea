package hoverboard.rewrite

import hoverboard.Name
import hoverboard.term.{AppView, Fix, Var}

import scalaz.IList

class Critiquer(simplifier: => Simplifier) {

  // TODO potential optimisation: entire process could fail when critique fails
  def run(env: Env, blockedRipple: Ripple): Ripple = {
    import blockedRipple._

    if (skeletons.isEmpty) {
      blockedRipple
    } else if (skeletons.length == 1 && skeletons.toList.head =@= goal) {
      val genVar = Name.fresh("χ")
      Ripple(IList(Var(genVar)), Var(genVar), generalisation ++! goal / genVar)
    } else {
      if (env.alreadySeen(goal))
        blockedRipple
      else {
        simplifier.run(env, goal) match {
          case AppView(goalFun: Fix, goalArgs) =>
            goalFun.fissionConstructorContext match {
              case Some((fissionedCtx, fissionedFix)) if env.rewriteDirection.canIncrease =>
                val newRipple = blockedRipple.copy(goal = fissionedFix.apply(goalArgs))
                val critiquedRipple = run(env.havingSeen(goal), newRipple)
                critiquedRipple.mapGoal(fissionedCtx.apply)
              case _ =>
                if (skeletons.length == 1 &&
                    simplifier.proveLeq(env, goal, skeletons.headOption.get)) {
                  val genVar = Name.fresh("χ(eq)")
                  Ripple(IList(Var(genVar)), Var(genVar), generalisation ++! skeletons.headOption.get / genVar)
                } else {
                  blockedRipple
                }
            }
          case _ =>
            blockedRipple
        }
      }
    }
  }
}
