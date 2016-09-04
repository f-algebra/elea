package hoverboard

import hoverboard.term._

import scala.annotation.tailrec
import scalaz.Scalaz._
import scalaz._

/**
  * `supercompile` -> `ripple` -> `critique` -> `supercompiler`,
  * where -> means _calls_
  */
class Supercompiler {

  import Supercompiler._

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

  final def dive(env: Env, fold: Fold)(skeleton: Term, goal: Term): (Term, Substitution) = {
    val (rippledGoalSubterms, rippleSubs) = goal.immediateSubterms.map(ripple(env, fold)(skeleton, _)).unzip
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

  final def couple(env: Env, fold: Fold)(skeleton: Term, goal: Term): (Term, Substitution) = {
    val (ctx, skelSub, goalSub) = skeleton ᴨ goal
    val rippled: IMap[Name, (Term, Substitution)] =
      skelSub.toMap.intersectionWith(goalSub.toMap)(ripple(env, fold))
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

  // TODO potential optimisation: entire process could fail when critique fails
  def critique(env: Env)(skeletons: ISet[Term], goal: Term): (Term, Substitution) = {
    if (skeletons.isEmpty) {
      (goal, Substitution.empty)
    } else {
      val overlap = ISet.fromFoldable(goal.subterms).intersection(skeletons)
      if (!overlap.isEmpty) {
        val matchedSkeletons = overlap.toIList
        ???
      } else {
        lazy val failure = (goal, Substitution.empty)
        if (env.alreadySeen(goal))
          failure
        else {
          /*supercompile(goal)*/ goal match {
            case AppView(goalFun: Fix, goalArgs) =>
              goalFun.fissionConstructorContext match {
                case Some((fissionedCtx, fissionedFix)) =>
                  val newGoal = fissionedCtx.apply(fissionedFix).apply(goalArgs).drive
                  critique(env.havingSeen(goal))(skeletons, newGoal)
                case None =>
                  failure
              }
            case _ =>
              failure
          }
        }
      }
    }
  }

  def ripple(env: Env, fold: Fold)(skeleton: Term, goal: Term): (Term, Substitution) =
    if (!couplesWith(skeleton, goal))
      dive(env, fold)(skeleton, goal)
    else {
      val (coupledGoal, coupledSub) = couple(env, fold)(skeleton, goal)
      val rippledGoal = coupledGoal :/ coupledSub
      skeleton unifyLeft rippledGoal match {
        case Some(rippleUni) =>
          fold.from.unifyLeft(skeleton) match {
            case None =>
              val genVar = Name.fresh("ξ")
              (Var(genVar), rippledGoal / genVar)
            case Some(foldUni) =>
              ((fold.to :/ foldUni) :/ rippleUni, Substitution.empty)
          }
        case _ =>
          (coupledGoal, coupledSub)
      }
    }

  final def supercompile(term: Term): Term =
    supercompile(Env.empty, term)

  def supercompile(env: Env, term: Term): Term = {
    term.drive(env.rewriteEnv) match {
      case AppView(fun: Fix, args) =>
        if (fun.isFPPF(args))
          fun.apply(args)
        else {
          val cp = CriticalPair.of(fun, args)
          env.folds.find(_.criticalPair embedsInto cp) match {
            case None =>
              // No existing fold has a matching critical path, so we should continue unrolling
              val fold = Fold(cp, fun.apply(args))
              val newEnv = env.withFold(fold)
              val unfoldedTerm = fold.from.replace(cp.term, cp.termUnfolded).drive(env.rewriteEnv)
              val supercompiledTerm = supercompile(newEnv, unfoldedTerm)
              if (!supercompiledTerm.freeVars.contains(fold.foldVar)) {
                // If there are no folds we could also be trying to apply then supercompilation has completely failed
                // and we should just return the original term
                if (env.folds.isEmpty) {
                  fun.apply(args)
                } else {
                  supercompiledTerm
                }
              } else {
                val fixBody = Lam(fold.foldVar :: fold.args, supercompiledTerm)
                Fix(fixBody, fun.index)
                  .apply(fold.args.map(Var(_): Term))
                  .drive(env.rewriteEnv)
              }
            case Some(fold) =>
              // We've found a matching critical path, so it's time to ripple
              val (rippledTerm, rippleSub) = ripple(env, fold)(fold.from, term)
              rippledTerm :/ rippleSub
          }
        }
      case AppView(fun, args) if args.nonEmpty =>
        // Constructor or variable function, so supercompile the arguments
        App(fun, args.map(supercompile(env, _)))
      case leq: Leq =>
        supercompile(env, leq.smallerTerm) match {
          case FPPF(fun: Fix, argVars) =>
            // Apply the least-fixed-point rule
            val newSmallerTerm = fun
              .body
              .betaReduce(NonEmptyList(Lam(argVars, leq.largerTerm)))
              .apply(argVars.map(Var(_): Term))
            supercompile(env, Leq(newSmallerTerm, leq.largerTerm))
          case newSmallerTerm =>
            Leq(newSmallerTerm, leq.largerTerm).drive(env.rewriteEnv)
        }
      case term: Case =>
        // Descend into the branches of pattern matches
        val newBranches = term.branches.map(supercompileBranch(env, term.matchedTerm))
        val newMatchedTerm = supercompile(env, term.matchedTerm)
        term.copy(branches = newBranches, matchedTerm = newMatchedTerm).drive(env.rewriteEnv)
      case term =>
        term
    }
  }

  /**
    * Supercompile the body of a pattern match branch,
    * safely (without variable capture) adding the matched pattern into the environment
    */
  final def supercompileBranch(env: Env, matchedTerm: Term)(branch: Branch): Branch = {
    branch match {
      case branch: DefaultBranch =>
        branch.copy(body = supercompile(env, branch.body))
      case branch: PatternBranch =>
        val freshBranch = branch.avoidCapture(env.bindingsSet.union(matchedTerm.freeVars))
        branch.copy(body = supercompile(env.withMatch(matchedTerm, branch.pattern), branch.body))
    }
  }
}


object Supercompiler {

  case class Fold private(criticalPair: CriticalPair, from: Term, to: Term, foldVar: Name, args: IList[Name])

  object Fold {
    def apply(criticalPair: CriticalPair, from: Term): Fold = {
      val foldVar = Name.fresh("μ")
      val args = from.freeVars.toIList
      val to = Var(foldVar).apply(args.map(Var(_): Term))
      Fold(criticalPair, from, to, foldVar, args)
    }
  }

  case class Env(rewriteEnv: rewrite.Env,
                 folds: IList[Fold]) {

    def alreadySeen(term: Term): Boolean =
      rewriteEnv.alreadySeen(term)

    def havingSeen(term: Term): Env =
      copy(rewriteEnv = rewriteEnv.havingSeen(term))

    def withMatch(term: Term, pattern: Pattern): Env =
      copy(rewriteEnv = rewriteEnv.withMatch(term, pattern))

    def withBindings(bindings: ISet[Name]): Env =
      copy(rewriteEnv = rewriteEnv.withBindings(bindings))

    def withFold(fold: Fold): Env =
      copy(folds = folds :+ fold)

    def invertDirection: Env =
      copy(rewriteEnv = rewriteEnv.invertDirection)

    def bindingsSet: ISet[Name] = rewriteEnv.bindingsSet
  }

  object Env {
    def empty: Env =
      Env(rewrite.Env.empty, IList.empty)
  }

  class SubstitutionsNonUnifiableError(detailMsg: String) extends AssertionError(detailMsg)

}
