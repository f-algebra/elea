package hoverboard.term

import hoverboard._
import hoverboard.term.CriticalPair.Induction

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
    * Used to check whether we should continue unfolding fixed-points in [[hoverboard.rewrite.Supercompiler.supercompile()]].
    */
  def embedsInto(other: CriticalPair): Boolean =
    path embedsInto other.path
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
          case AppView(matchFix: Fix, _) if false && matchFix.fissionConstructorContext.isDefined =>
            CriticalPair
              .fission(term)
          case AppView(matchFix: Fix, matchArgs: IList[Term]) =>
            CriticalPair
              .of(matchFix, matchArgs)
              .extendPathWithMatch(term.index)
          case _ =>
            CriticalPair
              .induction(term)
        }
      case term: Case =>
        CriticalPair
          .caseSplit(term)
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

    // TODO remove this when I add fixed-point induction?
    def caseOf: Case
  }

  case class Induction private(caseOf: Case) extends Action {
    override def :/(sub: Substitution) = copy(caseOf = caseOf :/ sub)

    override def shouldFold = true

    override def apply(from: Term): Term =
      C(_ => from).applyToBranches(caseOf)
  }

  case class CaseSplit private(caseOf: Case) extends Action {
    override def :/(sub: Substitution) = copy(caseOf = caseOf :/ sub)

    override def shouldFold = false

    override def apply(from: Term): Term =
      C(_ => from).applyToBranches(caseOf)
  }

  case class Fission private(caseOf: Case) extends Action {
    override def :/(sub: Substitution) = copy(caseOf = caseOf :/ sub)

    override def shouldFold = false

    override def apply(from: Term): Term =
      C(_ => from).applyToBranches(caseOf)
  }

  def induction(caseOf: Case): CriticalPair =
    CriticalPair(IList(caseOf.index), Induction(caseOf))

  def caseSplit(caseOf: Case): CriticalPair =
    CriticalPair(IList(caseOf.index), CaseSplit(caseOf))

  def fission(caseOf: Case): CriticalPair =
    CriticalPair(IList(caseOf.index), Fission(caseOf))
}
