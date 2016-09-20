package hoverboard.term

import hoverboard._
import scalaz.IList

case class CriticalPair(
  path: IList[Case.Index],
  fix: Fix,
  args: IList[Term]) {

  def extendPath(idx: Case.Index): CriticalPair = copy(path = idx :: path)

  /**
    * The _critical term_
    */
  def term: Term = fix.apply(args)

  /**
    * Check whether the path of this pair is a sub-path of the path of an`other` pair.
    * Used to check whether we should continue unfolding fixed-points in [[Supercompiler.supercompile()]].
    */
  def embedsInto(other: CriticalPair): Boolean =
    path embedsInto other.path
}

object CriticalPair {
  def of(fix: Fix, args: IList[Term]): CriticalPair = {
    fix.unfold.apply(args).drive match {
      case term: Case if term.matchedTerm.leftmost.isInstanceOf[Fix] =>
        val AppView(matchFun: Fix, matchArgs: IList[Term]) = term.matchedTerm
        of(matchFun, matchArgs)
          .extendPath(term.index)
      case term: Case =>
        CriticalPair(IList(term.index), fix, args)
      case AppView(newFix: Fix, newArgs) if newFix.index == fix.index =>
        CriticalPair(IList(Case.Index.Epsilon), fix, args)
      case _ =>
        CriticalPair(IList.empty[Case.Index], fix, args)
    }
  }
}
