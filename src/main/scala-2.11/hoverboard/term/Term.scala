package hoverboard.term

import hoverboard._
import hoverboard.Name
import hoverboard.rewrite.Env

import scalaz.{Name => _, _}
import Scalaz._

// TODO: Since terms are immutable, could we get faster equality by first checking reference equality?
// TODO: Make context fissioning use the SCC unfolding logic, and remove unfolding from drive (it's only in there for fission)

abstract class Term extends TermLike[Term] {

  def apply(args: Term*): Term =
    apply(IList(args: _*))

  def apply(args: IList[Term]): Term =
    NonEmptyList
      .lift[Term, Term](args => App(this, args))(args)
      .getOrElse(this)

  def apply(context: Context): Context =
    C(gap => this.apply(context.apply(Var(gap))))

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
  def driveHeadCase(env: Env, enclosingCase: Case): Term = enclosingCase.copy(matchedTerm = this)

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

  def flattenLam: (IList[Name], Term) = (IList.empty, this)

  def mapBranchesWithBindings(f: (ISet[Name], Term) => Term): Term =
    f(ISet.empty[Name], this)

  final def strictlyEmbedsInto(other: Term): Boolean =
    this.embedsInto(other) && !other.embedsInto(this)

  final def mostSpecificGeneralisation(other: Term): (Term, Substitution, Substitution) =
    this ᴨ other

  final def ᴨ(other: Term): (Term, Substitution, Substitution) =
    uzipWith(other)(_ ᴨ _) match {
      case None =>
        val newVar = Name.fresh("γ")
        (Var(newVar), this / newVar, other / newVar)
      case Some(msgs) =>
        val (subterms, thisSubs, otherSubs) = msgs.unzip3
        // If these unions fail then the fresh variable creator is broken
        // since all of these substitutions should have disjoint domains
        val Some(thisSub) = Substitution.union(thisSubs)
        val Some(otherSub) = Substitution.union(otherSubs)
        val newCtx = this.withImmediateSubterms(subterms)
        val mergeable = !thisSubs.isEmpty && subterms.all {
          case Var(n) => thisSub.boundVars.contains(n)
          case _ => false
        }
        if (mergeable) {
          val newVar = Name.fresh("γ")
          (Var(newVar), (newCtx :/ thisSub) / newVar, (newCtx :/ otherSub) / newVar)
        } else {
          (newCtx, thisSub, otherSub)
        }
    }

  /**
    * Replace occurrences (modulo alpha-equality) of `from` with `to` in this term and all its sub-terms
    */
  final def replace(from: Term, to: Term): Term =
    if (this =@= from)
      to
    else {
      mapImmediateSubtermsWithBindings {
        case (bindings, term) if from.freeVars.intersection(bindings).isEmpty =>
          term.replace(from, to)
        case (bindings, term) =>
          term
      }
    }

  /**
    * Generalise the given list of terms within this term.
    */
  final def generalise(termsToGeneralise: IList[Term]): (Term, IList[Name]) =
    termsToGeneralise.foldLeft((this, IList.empty[Name])) { case ((genTerm, genVars), termToGen) =>
      val genVar = Name.fresh("γ")
      (genTerm.replace(termToGen, Var(genVar)), genVars :+ genVar)
    }

  /**
    * Unfolds the outermost fixed-point of this term, if one exists.
    */
  def unfold: Term = this
}
