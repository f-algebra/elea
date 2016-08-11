package hoverboard.term

import hoverboard.rewrite.Env
import hoverboard._

import scalaz.{Name => _, _}
import Scalaz._

case class Case(matchedTerm: Term, branches: NonEmptyList[Branch], index: Case.Index) extends Term {

  override def driveHead(env: Env): Term =
    if (branches.all(_.body == Bot))
      Bot
    else
      matchedTerm.driveHeadCase(env, this)

  def driveBranches(env: Env): Case =
    copy(branches = branches.map(_.drive(env, matchedTerm)))

  override protected def driveSubterms(env: Env): Term =
    copy(matchedTerm = matchedTerm.drive(env)).driveBranches(env)

  // Floating pattern matches out of function position
  override def driveHeadApp(env: Env, args: NonEmptyList[Term]): Term =
    C(x => App(Var(x), args))
      .applyToBranches(this)
      .driveBranches(env)

  // Case-case disributivity rule
  override def driveHeadCase(env: Env, enclosingCase: Case): Term =
    C(x => enclosingCase.copy(matchedTerm = Var(x)))
      .applyToBranches(this)
      .driveBranches(env)

  override def :/(sub: Substitution): Term =
    Case(matchedTerm :/ sub, branches.map(_ :/ sub), index)

  def unifyLeft(to: Term): Option[Substitution] =
    to match {
      case to: Case =>
        val matchedUni = matchedTerm unifyLeft to.matchedTerm
        Substitution.merge(matchedUni +: branches.fzipWith(to.branches)(_ unifyLeft _).list)
      case _ => None
    }

  def couplingRule(other: Term): Boolean =
    other match {
      case other: Case =>
        matchedTerm.embedsInto(other) &&
          branches.fzipWith(other.branches)(_ couplingRule _).all(x => x)
      case _ => false
    }

  def mapImmediateSubtermsWithBindings(f: (ISet[Name], Term) => Term): Term =
    copy(matchedTerm = f(ISet.empty, matchedTerm),
      branches = branches.map(_.mapImmediateSubtermsWithBindings(f)))

  override def toString = {
    s"case[$index] $matchedTerm" +
      branches.map("\n" + _.toString).concatenate.indent +
      "\nend"
  }

  def zip(other: Term): Option[IList[(Term, Term)]] = {
    def resolve(xs: IList[Option[IList[(Term, Term)]]]): Option[IList[(Term, Term)]] =
      xs.sequence.map(_.flatten)

    other match {
      case other: Case if branches.size == other.branches.size =>
        resolve(branches.fzipWith(other.branches)(_ zip _).list).map((matchedTerm, other.matchedTerm) +: _)
      case _ =>
        None
    }
  }

  def uzip(other: Term): Option[IList[(Term, Term)]] = {
    def resolve(xs: IList[Option[IList[(Term, Term)]]]): Option[IList[(Term, Term)]] =
      xs.sequence.map(_.flatten)

    other match {
      case other: Case if branches.size == other.branches.size =>
        resolve(branches.fzipWith(other.branches)(_ uzip _).list).map((matchedTerm, other.matchedTerm) +: _)
      case _ =>
        None
    }
  }

  def arbitraryOrderingNumber: Int = 2

  def order(other: Term): Ordering =
    other match {
      case other: Case =>
        (matchedTerm ?|? other.matchedTerm) |+| branches.fzipWith(other.branches)(_ ?|? _).concatenate
      case _ =>
        arbitraryOrderingNumber ?|? other.arbitraryOrderingNumber
    }

  override def deepBranches: IList[Term] =
    branches.list.flatMap(_.body.deepBranches)

  override def mapBranchesWithBindings(f: (ISet[Name], Term) => Term): Term = {
    val mappedBranches = branches.map(_.mapImmediateSubtermsWithBindings { (moreBindings, term) =>
        def f2(bindings: ISet[Name], term: Term) = f(bindings.union(moreBindings), term)
        term.mapBranchesWithBindings(f2)
      })
    copy(branches = mappedBranches)
  }

  override def freshen =
    copy(branches = branches.map(_.freshen))

  override def freshenIndices: Case = copy(index = Case.freshIndex)
}

object Case {
  case class Index(name: Name) {
    override def toString: String = name.toString
  }

  def freshIndex = Index(Name.fresh("κ"))
}
