package hoverboard

import hoverboard.term._
import scalaz._
import Scalaz._

object Supercompiler {
  def ripple(skeleton: Term, goal: Term): Option[(Term, Substitution)] =
    skeleton match {
//      case skeleton: App =>
//        goal match {
//            // Coupling step
//          case goalFun: Fix if skeletonFun.index == goalFun.index =>
//
//        }

      case _ =>
        None  // Probably an assertion error
    }

  def unfold(term: App): Term = {
    // Should be used like, while !terms.any(_ embedsInto x) x = x.unfold
    term.fun match {
      case fix: Fix =>
        val strictArgs = fix.strictArgs.filter { i =>
          term.args.list.index(i) match {
            case Some(App(f, _)) => f.isInstanceOf[Fix]
            case _ => false
          }
        }
        if (strictArgs.isEmpty)
          App(fix.unfold, term.args)
        else {
          val newArgs = strictArgs.foldl[IList[Term]](term.args.list) { args => n =>
            args.setAt(n, unfold(args.index(n).get.asInstanceOf[App]))
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
