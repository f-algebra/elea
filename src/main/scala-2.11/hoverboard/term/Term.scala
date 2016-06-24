package hoverboard.term

import hoverboard.Name
import hoverboard.rewrite.Env

import scalaz._
import Scalaz._

// TODO: Since terms are immutable, could we get faster equality by first checking reference equality?

abstract class Term extends TermLike[Term] {

  def apply(args: Term*): Term =
    apply(IList(args: _*))

  def apply(args: IList[Term]): Term =
    NonEmptyList
      .lift[Term, Term](args => App(this, args))(args)
      .getOrElse(this)

  /**
    * Drive (simplify) a term all of whose sub-terms have already been driven
    */
  def driveHead(env: Env): Term = this

  /**
    * Drive given that this is a function applied to the provided arguments,
    * and when all sub-terms have already been driven.
    */
  def driveHeadApp(env: Env, args: NonEmptyList[Term]): Term = App(this, args)

  /**
    * Drive given that this is a `matchedTerm` in a [[Case]] with the provided branches,
    * and when all sub-terms have already been driven.
    */
  def driveHeadCase(env: Env, branches: NonEmptyList[Branch]): Term = Case(this, branches)

  /**
    * Drive (simplify) all sub-terms of a term
    */
  protected def driveSubterms(env: Env): Term =
    mapImmediateSubterms(_.drive(env))

  /**
    * Drive (simplify). Driving is the name of the less complex rewriting step
    * which occurs within supercompilation.
    * @param env Environment the rewrite occurs within, e.g. what [[hoverboard.rewrite.Direction]] we are preserving
    */
  def drive(env: Env): Term = {
    val result =
      if (env.alreadySeen(this))
        this
      else driveSubterms(env).driveHead(env)
    result
  }

  final def drive: Term = drive(Env.empty)

  def /(from: Name): Substitution =
    Substitution(from -> this)

  /**
    * Apply beta-reduction as if this term were applied to the given set of arguments
    */
  def betaReduce(args: NonEmptyList[Term]): Term =
    App(this, args)

  /**
    * Whether this term homeomorphically embeds into the `other` term.
    */
  def embedsInto(other: Term): Boolean =
    other.immediateSubterms.any(this.embedsInto) || couplingRule(other)

  def withName(name: String) = this

  override def equals(other: Any): Boolean =
    other match {
      case other: Term => order(other) == Ordering.EQ
      case _ => false
    }

  final def terms: IList[Term] = this +: subterms

  final def mapTermsContaining(vars: ISet[Name])(f: Term => Term): Term =
    if (vars.all(freeVars.contains))
      f(this.mapSubtermsContaining(vars)(f))
    else
      this

  def leftmost: Term = this

  def deepBranches: IList[Term] = IList(this)

  final def explore: IList[Term] = {
    val unfolded = this
      .mapSubterms {
        case fix: Fix => fix.body.apply(fix.body.apply(Bot))
        case other => other
      }
      .drive
    unfolded.deepBranches
  }

  final def exploreSet: ISet[Term] =
    ISet.fromFoldable(explore)

  def stripContext(context: Context): Option[Term] = {
    var anyFailures = false
    val stripped = mapBranchesWithBindings { (bindings, term) =>
        if (!bindings.intersection(context.freeVars).isEmpty) {
          anyFailures = true
          term
        } else context.unifyLeft(term) match {
          case Some(subst) if subst.boundVars == ISet.singleton(Name("_")) =>
            subst(Var("_"))
          case _ =>
            anyFailures = true
            term
        }
      }
    if (anyFailures)
      None
    else
      Some(stripped)
  }

  def flattenLam: (IList[Name], Term) = (IList.empty, this)

  def mapBranchesWithBindings(f: (ISet[Name], Term) => Term): Term =
    f(ISet.empty[Name], this)

  final def strictlyEmbedsInto(other: Term): Boolean =
    this.embedsInto(other) && !other.embedsInto(this)
}
