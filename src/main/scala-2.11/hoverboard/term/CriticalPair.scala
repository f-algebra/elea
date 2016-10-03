package hoverboard.term

import hoverboard._
import scalaz.{ISet, IList}

case class CriticalPair(
  path: CriticalPath,
  fix: Fix,
  args: IList[Term]) {

  def :/(sub: Substitution) = copy(path = path :/ sub)

  def extendPathWithMatch(idx: Case.Index): CriticalPair =
    copy(path = CriticalPath.Match(idx, path))

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
    val fixVar = Name.fresh("f")
    lazy val argSubterms = ISet.unions(args.toList.map(arg => arg.freeSubtermSet.insert(arg)))
    val cp = fix.body.apply(Var(fixVar) :: args).reduce match {
      case term: Case
          if term.matchedTerm.leftmost.isInstanceOf[Fix] && argSubterms.contains(term.matchedTerm) =>
        val AppView(matchFun: Fix, matchArgs: IList[Term]) = term.matchedTerm
        of(matchFun, matchArgs)
          .extendPathWithMatch(term.index)
      case term: Case =>
        CriticalPair(CriticalPath.Terminal(term.matchedTerm), fix, args)
          .extendPathWithMatch(term.index)
      case term =>
        CriticalPair(CriticalPath.Terminal(term), fix, args)
    }
    cp :/ fix / fixVar
  }
}
