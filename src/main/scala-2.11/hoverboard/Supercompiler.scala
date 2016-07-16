package hoverboard

import hoverboard.term._

import scalaz.Scalaz._
import scalaz._

object Supercompiler {

  case class Fold(from: Term, to: Term)

  case class Env(rewriteEnv: rewrite.Env,
                 folds: IList[Fold]) {

    def alreadySeen(term: Term): Boolean =
      rewriteEnv.alreadySeen(term)

    def havingSeen(term: Term): Env =
      copy(rewriteEnv = rewriteEnv.havingSeen(term))
  }

  object Env {
    def empty: Env =
      Env(rewrite.Env.empty, IList.empty)
  }

  class SubstitutionsNonUnifiableError(detailMsg: String) extends AssertionError(detailMsg)

  final def couplesWith(skeleton: Term, goal: Term): Boolean =
    (skeleton, goal) match {
      case (AppView(skelFun, skelArgs), AppView(goalFun, goalArgs)) if skelArgs.length == goalArgs.length =>
        (skelFun, goalFun) match {
          case (skelFun: Fix, goalFun: Fix) => skelFun.index == goalFun.index
          case (skelFun: Var, goalFun: Var) => skelFun == goalFun
          case _ => false
        }
      case _ =>
        false
    }

  final def dive(env: Env)(skeleton: Term, goal: Term): (Term, Substitution) = {
    val (rippledGoalSubterms, rippleSubs) = goal.immediateSubterms.map(ripple(env)(skeleton, _)).unzip
    Substitution.union(rippleSubs) match {
      case None =>
        throw new SubstitutionsNonUnifiableError(s"$skeleton dive $goal")
      case Some(unifiedSubs) =>
        (goal.withImmediateSubterms(rippledGoalSubterms), unifiedSubs)
    }
  }

  final def mergeRipples(ripples: IMap[Name, (Term, Substitution)]): (Substitution, Substitution) =
    Substitution.union(ripples.values.map(_._2).toIList) match {
      case None =>
        throw new SubstitutionsNonUnifiableError(s"$ripples")
      case Some(mergedGenSub) =>
        val mergedCtxSub = Substitution.fromMap(ripples.map(_._1))
        (mergedCtxSub, mergedGenSub)
    }

  final def couple(env: Env)(skeleton: Term, goal: Term): (Term, Substitution) = {
    val (ctx, skelSub, goalSub) = skeleton ⨅ goal
    val rippled: IMap[Name, (Term, Substitution)] =
      skelSub.toMap.intersectionWith(goalSub.toMap)(ripple(env))
    val (mergedCtxSub, mergedGenSub) = mergeRipples(rippled)
    // Simplifying assumption because I'm too hungover to figure out the general case
    mergedCtxSub.boundVars.toList match {
      case Seq(ctxVar) =>
        val newSkeletons = mergedGenSub.boundVars.map(x => ctx :/ Var(x) / ctxVar)
        val (critiquedCtx, critiquedSub) = critique(env)(newSkeletons, ctx :/ mergedCtxSub)
        val fullSub = (critiquedSub ++ mergedGenSub).getOrElse {
          throw new SubstitutionsNonUnifiableError(s"$skeleton ++ $goal")
        }
        (critiquedCtx, fullSub)
      case _ =>
        (ctx :/ mergedCtxSub, mergedGenSub)
    }
  }

  // TODO could fail when critique fails, if this is a hotspot
  final def critique(env: Env)(skeletons: ISet[Term], goal: Term): (Term, Substitution) = {
    lazy val failure = (goal, Substitution.empty)
    if (env.alreadySeen(goal))
      failure
    else {
      supercompile(goal) match {
        case AppView(goalFun: Fix, goalArgs) =>
          goalFun.fissionConstructorContext match {
            case Some(fissionedFun) =>
              failure
            // critique(env.havingSeen(goal), skeletons, )
            case None =>
              failure
          }
        case _ =>
          failure
      }
    }
  }

  final def ripple(env: Env)(skeleton: Term, goal: Term): (Term, Substitution) =
    skeleton unifyLeft goal match {
      case Some(unifier) if skeleton.indices.isSubsetOf(goal.indices) =>
        val genVar = Name.fresh("ξ")
        (Var(genVar), goal / genVar)
      case _ =>
        if (couplesWith(skeleton, goal))
          couple(env)(skeleton, goal)
        else
          dive(env)(skeleton, goal)
    }

  final def unfold(term: Term): Term = {
    // Should be used like, while !terms.any(_ embedsInto x) x = x.unfold
    term match {
      case AppView(fix: Fix, args) =>
        val strictArgs = fix.strictArgIndices.filter { i =>
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

  def supercompile(term: Term): Term = ???
   // supercompile(Env.empty, term)

//  def supercompile(env: Env, term: Term): Term = {
//    term match {
//      case AppView(fun: Fix, args: IList[Term]) if !fun.isFPPF(args) =>
//
//      case _ =>
//        term
//    }
//  }
}
