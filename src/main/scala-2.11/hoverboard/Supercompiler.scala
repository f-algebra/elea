package hoverboard

import hoverboard.term._

import scalaz.Scalaz._
import scalaz._

object Supercompiler {

  class SubstitutionsNonUnifiableError(detailMsg: String) extends AssertionError(detailMsg)

  final def couplesWith(skeleton: Term, goal: Term): Boolean =
    (skeleton, goal) match {
      case (AppView(skelFun: Fix, skelArgs), AppView(goalFun: Fix, goalArgs)) =>
        skelFun.index == goalFun.index && skelArgs.length == goalArgs.length
      case _ =>
        false
    }

  final def dive(skeleton: Term, goal: Term): (Term, Substitution) = {
    val (rippledGoalSubterms, rippleSubs) = goal.immediateSubterms.map(ripple(skeleton, _)).unzip
    Substitution.union(rippleSubs) match {
      case None =>
        throw new SubstitutionsNonUnifiableError(s"$skeleton dive $goal")
      case Some(unifiedSubs) =>
        (goal.withImmediateSubterms(rippledGoalSubterms), unifiedSubs)
    }
  }

  final def couple(skeleton: Term, goal: Term): (Term, Substitution) = {
    val (ctx, skelSub, goalSub) = skeleton ⨅ goal
    val rippledWavefronts: IMap[Name, (Term, Substitution)] =
      skelSub.toMap.intersectionWith(goalSub.toMap)(ripple)
    Substitution.union(rippledWavefronts.values.map(_._2).toIList) match {
      case None =>
        throw new SubstitutionsNonUnifiableError(s"$skeleton dive $goal")
      case Some(mergedGenSub) =>
        val mergedCtxSub = Substitution.fromMap(rippledWavefronts.map(_._1))
        (ctx :/ mergedCtxSub, mergedGenSub)
    }
  }

  final def ripple(skeleton: Term, goal: Term): (Term, Substitution) =
    skeleton unifyLeft goal match {
      case Some(unifier) if skeleton.indices.isSubsetOf(goal.indices) =>
        val genVar = Name.fresh("ξ")
        (Var(genVar), goal / genVar)
      case _ =>
        if (couplesWith(skeleton, goal))
          couple(skeleton, goal)
        else
          dive(skeleton, goal)
    }

  final def unfold(term: Term): Term = {
    // Should be used like, while !terms.any(_ embedsInto x) x = x.unfold
    term match {
      case AppView(fix: Fix, args) =>
        val strictArgs = fix.strictArgs.filter { i =>
          args.index(i) match {
            case Some(AppView(f: Fix, _)) => true
            case _ => false
          }
        }
        if (strictArgs.isEmpty)
          fix.unfold.apply(args)
        else {
          val newArgs = strictArgs.foldl[IList[Term]](args) { args => n =>
            args.setAt(n, unfold(args.index(n).get))
          }
          fix.apply(newArgs)
        }
      case _ =>
        throw new IllegalArgumentException("Can only unfold a term with a fixed-point leftmost")
    }

  }

  def run(term: App): Term = {
    require(term.fun.isInstanceOf[Fix])
    ???
  }
}
