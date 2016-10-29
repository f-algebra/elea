package hoverboard.term

import hoverboard._
import scalaz.{ISet, IList}

// TODO get rid of terminal for now, it's pointless since you know Zeno doesn't need it,
// might be needed for co-induction though
// Also, you can make fix indices unique thingies now, since it's just used for the coupling check

case class CriticalPair(
  path: CriticalPath,
  caseOf: Case) {

  def :/(sub: Substitution) = copy(path = path :/ sub)

  def extendPathWithMatch(idx: Case.Index): CriticalPair =
    copy(path = CriticalPath.Match(idx, path))

  /**
    * Check whether the path of this pair is a sub-path of the path of an`other` pair.
    * Used to check whether we should continue unfolding fixed-points in [[Supercompiler.supercompile()]].
    */
  def embedsInto(other: CriticalPair): Boolean =
    path embedsInto other.path
}

object CriticalPair {
  def of(fix: Fix, args: IList[Term]): Option[CriticalPair] = {
    val fixVar = Name.fresh("f")
    val fixArgSubterms = ISet.unions(args.toList.map(arg => arg.freeSubtermSet.insert(arg)))
      .toList
      .filter(_.leftmost.isInstanceOf[Fix])
    val cp = fix.body.apply(Var(fixVar) :: args).reduce match {
      case term: Case
          if term.matchedTerm.leftmost.isInstanceOf[Fix] &&
            fixArgSubterms.exists(_ =@= term.matchedTerm) =>
        val AppView(matchFun: Fix, matchArgs: IList[Term]) = term.matchedTerm
        of(matchFun, matchArgs)
          .map(_.extendPathWithMatch(term.index))
      case term: Case =>
        Some(CriticalPair(CriticalPath.Terminal(term.matchedTerm), term))
            .map(_.extendPathWithMatch(term.index))
      case _ => None
    }
    cp.map(_ :/ fix / fixVar)
  }

  def unapply(term: Term): Option[(Fix, IList[Term], CriticalPair)] =
    term match {
      case AppView(fix: Fix, args) =>
        CriticalPair.of(fix, args).map((fix, args, _))
      case _ =>
        None
    }
}
