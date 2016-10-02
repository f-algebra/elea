package hoverboard.term

import hoverboard.rewrite.Env
import hoverboard._

import scalaz.{Name => _, _}
import Scalaz._

case class Case(matchedTerm: Term, branches: NonEmptyList[Branch], index: Case.Index) extends Term {

  override def reduceHead(env: Env): Term =
    if (branches.all(_.body == Bot))
      Bot
    else if (branches.all(_.isIdentity(matchedTerm)))
      matchedTerm
    else
      matchedTerm.reduceHeadCase(env, this)

  final def reduceBranches(env: Env): Case =
    copy(branches = branches.map(_.reduce(env, matchedTerm)))

  /**
    * Re-reduce after branches have been modified, but when we know the matched term has not been altered.
    */
  final def reduceIgnoringMatchedTerm(env: Env): Term =
    reduceBranches(env).reduceHead(env)

  override protected def reduceSubterms(env: Env): Term =
    copy(matchedTerm = matchedTerm.reduce(env)).reduceBranches(env)

  // Floating pattern matches out of function position
  override def reduceHeadApp(env: Env, args: NonEmptyList[Term]): Term =
    C(x => App(Var(x), args))
      .applyToBranches(this)
      .reduceIgnoringMatchedTerm(env)

  // Case-case disributivity rule
  override def reduceHeadCase(env: Env, enclosingCase: Case): Term =
    C(x => enclosingCase.copy(matchedTerm = Var(x)))
      .applyToBranches(this)
      .reduceIgnoringMatchedTerm(env)

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
    (s"\ncase[$index] $matchedTerm" +
      branches.map("\n" + _.toString).concatenate +
      "\nend").indent
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

  override def freshenIndices: Case = copy(index = Case.Index.fresh)
}

object Case {
  sealed trait Index

  object Index {
    def fromName(name: Name): Index = Named(name)

    def fresh: Index = Named(Name.fresh("κ"))

    case class Named(name: Name) extends Index  {
      override def toString: String = name.toString
    }
  }
}
