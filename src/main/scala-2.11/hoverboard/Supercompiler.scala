package hoverboard

import hoverboard.term._

import scalaz.Scalaz._
import scalaz._

object Supercompiler {

  final def ripple(skeleton: Term, goal: Term): Option[(Term, Substitution)] =
    (skeleton, goal) match {
      case (AppView(skelFun: Fix, skelArgs), AppView(goalFun: Fix, goalArgs)) =>
        // Coupling step
        if (skelFun.index == goalFun.index) {
          require(skelArgs.length == goalArgs.length, s"Should have equal argument counts: $skeleton vs. $goal")
          val rippledArgs = skelArgs.fzipWith(goalArgs)(ripple)
          if (rippledArgs.any(_.isEmpty))
            None
          else {
            None
          }
        }
        // Diving step
        else {
          None
        }
      case _ =>
        None // Probably an assertion error
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
