package hoverboard.term

import scalaz.IList

case class CriticalPair(
  path: IList[Case.Index],
  fix: Fix,
  args: IList[Term]) {

  def extendPath(idx: Case.Index): CriticalPair = copy(path = idx :: path)

  def term: Term = fix.apply(args)
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
      case _ =>
        CriticalPair(IList.empty[Case.Index], fix, args)
    }
  }
}
