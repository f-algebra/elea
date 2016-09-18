package hoverboard

import hoverboard.term._

import scala.annotation.tailrec
import scalaz.Scalaz._
import scalaz._

/**
  * `supercompile` calls `ripple` calls `critique` calls `supercompile`
  */
class Supercompiler {

  import Supercompiler._

  final private def dive(env: Env, skeleton: Term, goal: Term): (IList[Term], Term, Substitution) = {
    val (rippledSkeletons, rippledGoalSubterms, rippleSubs) = goal.immediateSubterms.map(ripple(env, skeleton, _)).unzip3
    val divedSub = Substitution.unionDisjoint(rippleSubs)
    val divedSkeletons = rippledSkeletons.concatenate
    val divedGoal = goal.withImmediateSubterms(rippledGoalSubterms)
    (divedSkeletons, divedGoal, divedSub)
  }

  def ripple(skeleton: Term, goal: Term): (IList[Term], Term, Substitution) =
    ripple(Env.empty, skeleton, goal)

  def ripple(env: Env, skeleton: Term, goal: Term): (IList[Term], Term, Substitution) =
    (skeleton, goal) match {
      case (skeleton: Var, goal: Var) =>
        (IList(skeleton), skeleton, goal / skeleton.name)
      case (AppView(skelFix: Fix, skelArgs: IList[Term]), AppView(goalFix: Fix, goalArgs: IList[Term]))
          if skelArgs.length == goalArgs.length && skelFix.index == goalFix.index =>
        val (rippledArgSkeletons: IList[IList[Term]], rippledArgGoals: IList[Term], rippledArgSubs: IList[Substitution]) =
          skelArgs.fzipWith(goalArgs)(ripple(env, _, _)).unzip3
        val rippledSkeletons: IList[Term] = (rippledArgSkeletons.sequence: IList[IList[Term]]).map { args => skelFix.apply(args) }
        val rippledGoal = goalFix.apply(rippledArgGoals)
        val rippleSub = Substitution.unionDisjoint(rippledArgSubs)
        val (critiquedSkeletons, critiquedGoal, critiqueSub) = critique(env, rippledSkeletons, rippledGoal)
        (critiquedSkeletons, critiquedGoal, rippleSub ++! critiqueSub)
      case _ =>
        dive(env, skeleton, goal)
    }
//
//  final def mergeRipples(ripples: IMap[Name, (Term, Substitution)]): (Substitution, Substitution) =
//    Substitution.union(ripples.values.map(_._2).toIList) match {
//      case None =>
//        throw new SubstitutionsNonUnifiableError(s"$ripples")
//      case Some(mergedGenSub) =>
//        val mergedCtxSub = Substitution.fromMap(ripples.map(_._1))
//        (mergedCtxSub, mergedGenSub)
//    }

//  final def couple(env: Env, skeleton: Term, goal: Term): (IList[Term], Term, Substitution) = {
//    val (ctx, skelSub, goalSub) = skeleton ᴨ goal
//    val rippled: IMap[Name, (Term, Substitution)] =
//      skelSub.toMap.intersectionWith(goalSub.toMap)(ripple(env, _, _))
//    val (mergedCtxSub, mergedGenSub) = mergeRipples(rippled)
//    // Simplifying assumption because I'm too hungover to figure out the general case
//    mergedCtxSub.boundVars.toList match {
//      case Seq(ctxVar) =>
//        val newSkeletons = mergedGenSub.boundVars.map(x => ctx :/ Var(x) / ctxVar)
//        val (critiquedCtx, critiquedSub) = critique(env, newSkeletons, ctx :/ mergedCtxSub)
//        val fullSub = (mergedGenSub ++ critiquedSub).getOrElse {
//          throw new SubstitutionsNonUnifiableError(s"$skeleton ++ $goal")
//        }
//        (critiquedCtx, fullSub)
//      case _ =>
//        (ctx :/ mergedCtxSub, mergedGenSub)
//    }
//  }

  // TODO potential optimisation: entire process could fail when critique fails
  def critique(env: Env, skeletons: IList[Term], goal: Term): (IList[Term], Term, Substitution) = {
    if (skeletons.isEmpty || skeletons.length == 1 && skeletons.toList.head =@= goal) {
      val genVar = Name.fresh("χ")
      (IList(Var(genVar)), Var(genVar), goal / genVar)
    } else {
      lazy val failure = (skeletons, goal, Substitution.empty)
      if (env.alreadySeen(goal))
        failure
      else {
        supercompile(env, goal) match {
          case AppView(goalFun: Fix, goalArgs) =>
            goalFun.fissionConstructorContext match {
              case Some((fissionedCtx, fissionedFix)) =>
                val newGoal = fissionedFix.apply(goalArgs)
                val (critiquedSkels, critiquedGoal, critiqueSub) = critique(env.havingSeen(goal), skeletons, newGoal)
                (critiquedSkels, fissionedCtx.apply(critiquedGoal), critiqueSub)
              case None =>
                failure
            }
          case _ =>
            failure
        }
      }
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
              val unfoldedTerm = fold.unfoldedFrom.drive(env.rewriteEnv)
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
                Fix(fixBody, cp.fix.index)
                  .apply(fold.args.map(Var(_): Term))
                  .drive(env.rewriteEnv)
              }
            case Some(fold) =>
              // We've found a matching critical path, so it's time to ripple
              val (rippledSkeletons, rippledGoal, rippleSub) = ripple(env, fold.from, term)
              val successfulRipples = rippledSkeletons
                .filter(_.isInstanceOf[Var])
                .map(_ :/ rippleSub)
                .map(fold.from.unifyLeft)
                .map(_.getOrElse { throw new AssertionError("Successful ripple was not unifiable with original skeleton")})
              successfulRipples.foldLeft(rippledGoal :/ rippleSub)((goal, sub) => goal replace (fold.from :/ sub, fold.to :/ sub))
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

  case class Fold (
       criticalPair: CriticalPair,
       nonReindexedFrom: Term) {

    val foldVar: Name = Name.fresh("μ")
    val index: Fix.Index = Fix.freshFiniteIndex
    val args: IList[Name] = nonReindexedFrom.freeVars.toIList
    val to: Term = Var(foldVar).apply(args.map(Var(_): Term))
    val reindexedFix: Fix = criticalPair.fix.copy(index = index)
    val from: Term = nonReindexedFrom.replace(criticalPair.term, reindexedFix.apply(criticalPair.args))
    val unfoldedFrom: Term = nonReindexedFrom.replace(criticalPair.term, reindexedFix.unfold.apply(criticalPair.args))
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
