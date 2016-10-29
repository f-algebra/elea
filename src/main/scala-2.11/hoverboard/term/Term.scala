package hoverboard.term

import hoverboard._
import hoverboard.Name
import hoverboard.rewrite.Env

import scalaz.{Name => _, _}
import Scalaz._

// TODO: Since terms are immutable, could we get faster equality by first checking reference equality?
// TODO: Make context fissioning use the SCC unfolding logic, and remove unfolding from reduce (it's only in there for fission)

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
    * reduce (simplify) a term all of whose sub-terms have already been reduced
    */
  def reduceHead(env: Env): Term = this

  /**
    * reduce given that this is a function applied to the provided arguments,
    * and when all sub-terms have already been reduced.
    */
  def reduceHeadApp(env: Env, args: NonEmptyList[Term]): Term = App(this, args)

  /**
    * reduce given that this is a `matchedTerm` in a [[Case]] with the provided branches,
    * and when all sub-terms have already been reduced.
    */
  def reduceHeadCase(env: Env, enclosingCase: Case): Term = enclosingCase.copy(matchedTerm = this)

  /**
    * reduce (simplify) all sub-terms of a term
    */
  protected def reduceSubterms(env: Env): Term =
    mapImmediateSubterms(_.reduce(env))

  /**
    * reduce (simplify). Driving is the name of the less complex rewriting step
    * which occurs within supercompilation.
    * @param env Environment the rewrite occurs within, e.g. what [[hoverboard.rewrite.Direction]] we are preserving
    */
  def reduce(env: Env): Term = {
    val result =
      if (env.alreadySeen(this))
        this
      else reduceSubterms(env).reduceHead(env)
    result
  }

  final lazy val reduce: Term = reduce(Env.empty)

  def /(from: Name): Substitution =
    Substitution(from -> this)

  /**
    * Apply beta-reduction as if this term were applied to the given set of arguments
    */
  def betaReduce(args: NonEmptyList[Term]): Term =
    App(this, args.list)

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
      .mapSubtermsAndThis {
        case fix: Fix => fix.body.apply(fix.body.apply(Bot))
        case other => other
      }
      .reduce
    unfolded.deepBranches
  }

  final def exploreSet: ISet[Term] =
    ISet.fromFoldable(explore)

  final def mapSubtermsAndThis(f: Term => Term): Term =
    f(mapSubterms(f))

  def flattenLam: (IList[Name], Term) = (IList.empty, this)

  def mapBranchesWithBindings(f: (ISet[Name], Term) => Term): Term =
    f(ISet.empty[Name], this)

  final def strictlyEmbedsInto(other: Term): Boolean =
    this.embedsInto(other) && !other.embedsInto(this)

  /**
    * Replace occurrences (modulo alpha-equality) of `from` with `to` in this term and all its sub-terms
    */
  def replace(from: Term, to: Term): Term =
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

  def =<(other: Term): Leq = Leq(this, other)

  final def caseIndexSet: ISet[Case.Index] =
    subtermSet.insert(this)
      .filter(_.isInstanceOf[Case])
      .map(_.asInstanceOf[Case].index)
}
