package hoverboard.rewrite

import hoverboard.term._
import hoverboard._
import hoverboard.term.CriticalPair.{CaseSplit, Induction}

import scalaz.Scalaz._
import scalaz.{Name => _, _}

/**
  * @param rippler Batman's most feared villain.
  */
class Supercompiler(rippler: Rippler, prover: Prover) extends Simplifier {

  import Supercompiler._

  override def run(env: Env, term: Term): Term =
    supercompile(env, IList.empty, term)

  /**
    * Supercompile a term whose sub-terms have already been supercompiled
    */
  private def supercompileHead(env: Env, folds: IList[Fold], term: Term): Term =
    term match {
      case Leq(smallerTerm, largerTerm) if smallerTerm =@= largerTerm =>
        Logic.Truth

      case Leq(smallerTerm: Case, largerTerm) if env.rewriteDirection.canIncrease =>
        val newBranches: NonEmptyList[Branch] = smallerTerm.branches.map {
          case branch: DefaultBranch =>
            branch.copy(body = supercompileHead(env, folds, Leq(branch.body, largerTerm)))
          case branch: PatternBranch =>
            val freshBranch = branch.avoidCapture(env.bindingsSet.union(smallerTerm.matchedTerm.freeVars))
            val newBody = Leq(freshBranch.body, largerTerm)
            val compiledBody = supercompileHead(env.withMatch(smallerTerm.matchedTerm, branch.pattern), folds, newBody)
            branch.copy(body = compiledBody)
        }
        smallerTerm
          .copy(branches = newBranches)
          .reduce(env.clearHistory)

      case Leq(smallerTerm, largerTerm: Case) if env.rewriteDirection.canDecrease =>
        val newBranches: NonEmptyList[Branch] = largerTerm.branches.map {
          case branch: DefaultBranch =>
            branch.copy(body = supercompileHead(env, folds, Leq(smallerTerm, branch.body)))
          case branch: PatternBranch =>
            val freshBranch = branch.avoidCapture(env.bindingsSet.union(largerTerm.matchedTerm.freeVars))
            val newBody = Leq(smallerTerm, freshBranch.body)
            val compiledBody = supercompileHead(env.withMatch(largerTerm.matchedTerm, branch.pattern), folds, newBody)
            branch.copy(body = compiledBody)
        }
        largerTerm
          .copy(branches = newBranches)
          .reduce(env.clearHistory)

      case Leq(FPPF(fix, args), largerTerm)
          if !env.alreadySeen(term) && (env.rewriteDirection.canIncrease || fix.isProductive) =>
        // Apply the least-fixed-point rule
        val newSmallerTerm = fix
          .body
          .betaReduce(NonEmptyList(Lam(args, largerTerm)))
          .apply(args.map(Var(_): Term))
        val newLeq = Leq(newSmallerTerm, largerTerm).reduce(env.clearHistory)
        val result = supercompile(env.havingSeen(term), IList.empty, newLeq)
        result

      case Leq(smallerTerm, FPPF(fix, args))
          if !env.alreadySeen(term) && (env.rewriteDirection.canDecrease || fix.isProductive) =>
        // The least fixed-point rule can be applied backwards for well-founded fixed-points
        val newLargerTerm = fix
          .body
          .betaReduce(NonEmptyList(Lam(args, smallerTerm)))
          .apply(args.map(Var(_): Term))
        val newLeq = Leq(smallerTerm, newLargerTerm).reduce(env.clearHistory)
        val result = supercompile(env.havingSeen(term), IList.empty, newLeq)
        result

      case _ if prover.unsatisfiable(env) =>
        Bot

      case other =>
        other
    }

  /**
    * @param folds We actually maintain two lists of already seen folds. The one in `env` is for ensuring termination,
    *              the one in this variable is for the folding step of supercompilation. So, this list is always
    *              a sub-list of the one in `env`.
    */
  private def supercompile(env: Env, folds: IList[Fold], term: Term): Term = {
    term.reduce(env.clearHistory) match {
      case FPPF(fun, args) =>
        // Job done
        fun.apply(args.map(Var(_): Term))

      case CriticalPair(fun: Fix, args, cp) =>
        val fold = Fold(cp, fun.apply(args))
        folds.find(_ embedsInto fold) match {
          case None if !env.alreadySeen(fold) =>
            // No existing fold has a matching critical path, so we should continue unrolling
            val newEnv = env
              .havingSeen(fold)       // Store this fold so it can be matched on later to ensure termination
              .clearMatches           // It is unsound to apply pattern matches from outside supercompilation, within the supercompilation step
            val expandedTerm = fold
              .expand
              .reduce(env.clearHistory)
            val newFolds = if (fold.shouldMemoise) fold :: folds else folds
            val supercompiledTerm = supercompile(newEnv, newFolds, expandedTerm)
            if (!fold.shouldMemoise || !supercompiledTerm.freeVars.contains(fold.foldVar)) {
              supercompiledTerm
            } else {
              val fixBody = Lam(fold.foldVar :: fold.args, supercompiledTerm)
              val result = Fix(fixBody, fun.index)
                .apply(fold.args.map(Var(_): Term))
                .reduce(env.clearHistory)
              result
            }

          case Some(existingFold) =>
            // We've found a matching critical path, so it's time to ripple
            val ripple = rippler.run(env, existingFold.from, term)
            val successfulRipples = ripple
              .skeletons
              .filter(_.isInstanceOf[Var])
              .flatMap { x => existingFold.from.unifyLeft(x :/ ripple.generalisation).toIList }
            val result = successfulRipples.foldLeft(ripple.goal :/ ripple.generalisation) { (goal, sub) =>
              goal.replace(existingFold.from :/ sub, existingFold.to :/ sub)
            }
            result

          case _ =>
            fun.apply(args)
        }
      case AppView(fun, args) if args.nonEmpty =>
        // Constructor or variable function, so supercompile the arguments
        App(fun, args.map(supercompile(env, folds, _)))

      case leq: Leq =>
        val compiledLargerTerm = run(env.invertDirection, leq.largerTerm)
        val compiledSmallerTerm = run(env, leq.smallerTerm)
        supercompileHead(env, IList.empty, Leq(compiledSmallerTerm, compiledLargerTerm))

      case term: Case =>
        // Descend into the branches of pattern matches
        val newMatchedTerm = supercompile(env, folds, term.matchedTerm)
        val newBranches: NonEmptyList[Branch] = term.branches.map {
          case branch: DefaultBranch =>
            branch.copy(body = supercompile(env, folds, branch.body))
          case branch: PatternBranch =>
            val freshBranch = branch.avoidCapture(env.bindingsSet.union(newMatchedTerm.freeVars))
            val newBody = supercompile(env.withMatch(newMatchedTerm, freshBranch.pattern), folds, freshBranch.body)
            freshBranch.copy(body = newBody)
        }
        val fissionedMatchedTerm = newMatchedTerm match {
          case AppView(matchFix: Fix, matchArgs) =>
            matchFix.fissionConstructorContext(matchArgs) match {
              case Some(fissionedTerm) =>
                fissionedTerm
              case _ =>
                newMatchedTerm
            }
          case _ =>
            newMatchedTerm
        }
        term
          .copy(branches = newBranches, matchedTerm = fissionedMatchedTerm)
          .reduce(env.clearHistory)
      case _ if prover.unsatisfiable(env) =>
        Bot
      case other =>
        other
    }
  }
}


object Supercompiler {

  case class Fold (
       criticalPair: CriticalPair,
       from: Term) {

    val foldVar: Name = Name.fresh("μ")
    val args: IList[Name] = from.freeVars.toIList
    val to: Term = Var(foldVar).apply(args.map(Var(_): Term))

    def path: IList[Case.Index] = criticalPair.path
    def action: CriticalPair.Action = criticalPair.action

    def expand: Term =
      criticalPair.action.apply(from)

    def shouldMemoise: Boolean =
      criticalPair.action.shouldFold

    def embedsInto(other: Fold): Boolean =
      if (path == other.path && action.sameTypeAs(other.action))
        from.embedsInto(other.from)
      else
        criticalPair.embedsInto(other.criticalPair)
  }
}
