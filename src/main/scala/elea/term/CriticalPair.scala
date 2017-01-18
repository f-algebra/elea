package elea.term

import elea._
import elea.term.CriticalPair.Induction

import scalaz.{ICons, IList, ISet}

// Also, you can make fix indices unique thingies now, since it's just used for the coupling check

case class CriticalPair(
  path: IList[Case.Index],
  action: CriticalPair.Action) {

  def isFoldable: Boolean = action.isInstanceOf[Induction]

  def :/(sub: Substitution): CriticalPair =
    copy(action = action :/ sub)

  def extendPathWithMatch(idx: Case.Index): CriticalPair =
    copy(path = idx :: path)

  /**
    * Are the first elements of the two critical paths equal to each other
    */
  def couplesWith(other: CriticalPair): Boolean =
    (path, other.path) match {
      case (ICons(x, _), ICons(y, _)) => x == y
      case _ => false
    }

  /**
    * Check whether the path of this pair is a sub-path of the path of an`other` pair.
    * Used to check whether we should continue unfolding fixed-points in [[elea.rewrite.Supercompiler.supercompile()]].
    */
  def embedsInto(other: CriticalPair): Boolean =
    action.sameTypeAs(other.action) &&
      path.embedsInto(other.path)
}

object CriticalPair {
  def of(fix: Fix, args: IList[Term]): CriticalPair = {
    val fixVar = Name.fresh("f")
    val fixArgSubterms = ISet
      .unions(args.toList.map(arg => arg.freeSubtermSet.insert(arg)))
      .toList
    val cp = fix.body.apply(Var(fixVar) :: args).reduce match {
      case term: Case if fixArgSubterms.exists(_ =@= term.matchedTerm) =>
        term.matchedTerm match {
          case AppView(matchFix: Fix, matchArgs) if matchFix.fissionConstructorContext.isDefined =>
            CriticalPair
              .fission(matchFix, matchArgs)
              .extendPathWithMatch(term.index)
          case AppView(matchFix: Fix, matchArgs: IList[Term]) =>
            CriticalPair
              .of(matchFix, matchArgs)
              .extendPathWithMatch(term.index)
          case _ =>
            CriticalPair
              .induction(term)
              .extendPathWithMatch(term.index)
        }
      case term: Case =>
        CriticalPair
          .caseSplit(term)
          .extendPathWithMatch(term.index)
      case _ =>
        throw new IllegalArgumentException(s"Term does not have critical pair: ${fix.apply(args)}")
    }
    cp :/ (fix / fixVar)
  }

  def unapply(term: Term): Option[(Fix, IList[Term], CriticalPair)] =
    term match {
      case AppView(fix: Fix, args) if fix.argCount == args.length =>
        Some(fix, args, CriticalPair.of(fix, args))
      case _ =>
        None
    }

  sealed trait Action {
    def :/(sub: Substitution): Action

    def shouldFold: Boolean

    def apply(from: Term): Term

    def sameTypeAs(other: Action): Boolean
  }

  case class Induction private(caseOf: Case) extends Action {
    override def :/(sub: Substitution) = copy(caseOf = caseOf :/ sub)

    override def shouldFold = true

    override def apply(from: Term): Term =
      C(_ => from).applyToBranches(caseOf)

    override def sameTypeAs(other: Action): Boolean =
      other.isInstanceOf[Induction]
  }

  case class CaseSplit private(caseOf: Case) extends Action {
    override def :/(sub: Substitution) = copy(caseOf = caseOf :/ sub)

    override def shouldFold = false

    override def apply(from: Term): Term =
      C(_ => from).applyToBranches(caseOf)

    override def sameTypeAs(other: Action): Boolean =
      other.isInstanceOf[CaseSplit]
  }

  case class Fission private(fix: Fix, args: IList[Term]) extends Action {
    require(fix.fissionConstructorContext.isDefined)

    override def :/(sub: Substitution) = copy(
      fix = (fix :/ sub).asInstanceOf[Fix],
      args = args.map(_ :/ sub))

    override def shouldFold = false

    override def apply(from: Term): Term =
      from.replace(fix.apply(args), fix.fissionConstructorContext(args).get)

    override def sameTypeAs(other: Action): Boolean =
      other.isInstanceOf[Fission]
  }

  def induction(caseOf: Case): CriticalPair =
    CriticalPair(IList.empty, Induction(caseOf))

  def caseSplit(caseOf: Case): CriticalPair =
    CriticalPair(IList.empty, CaseSplit(caseOf))

  def fission(fix: Fix, args: IList[Term]): CriticalPair =
    CriticalPair(IList.empty, Fission(fix, args))
}
