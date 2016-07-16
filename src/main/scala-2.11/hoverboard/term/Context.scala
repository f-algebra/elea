package hoverboard.term

import hoverboard.Name

import scalaz.{Name => _, _}
import Scalaz._

final case class Context private(gap: Name, context: Term) extends TermLike[Context] {
  def apply(term: Term): Term = context :/ term / gap

  def applyToBranches(caseOf: Case): Case = {
    val newBranches = caseOf.branches.map { branch =>
      branch.avoidCapture(freeVars).mapImmediateSubterms(apply)
    }
    caseOf.copy(branches = newBranches)
  }

  override protected def getFreeVars: ISet[Name] = context.freeVars.delete(gap)

  /**
    * Attempts to remove this context from the given term, returning what would be in the gap.
    * Descends into branches of pattern matches to strip contexts, hence it may increase definedness.
    * `Some(A) = C.strip(B) ==> C[A] >= B`
    */
  def strip(term: Term): Option[Term] = {
    var anyFailures = false
    val stripped = term.mapBranchesWithBindings { (bindings, term) =>
      if (!bindings.intersection(context.freeVars).isEmpty) {
        anyFailures = true
        term
      } else context.unifyLeft(term) match {
        case Some(sub) if sub.boundVars == ISet.singleton(gap) =>
          Var(gap) :/ sub
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

  override def toString = apply(Var("_")).toString

  def composeWith(inner: Context): Context =
    C(gap => this(inner(Var(gap))))

  def +(inner: Context): Context =
    composeWith(inner)

  override def =@=(other: Context): Boolean =
    context =@= other.context :/ Var(gap) / other.gap

  override def :/(substitution: Substitution): Context =
    copy(context = context :/ substitution)

  override def unifyLeft(to: Context): Option[Substitution] =
    context unifyLeft (to.context :/ Var(gap) / to.gap)

  override def order(other: Context): Ordering =
    (gap ?|? other.gap) |+| (context ?|? other.context)

  override def uzip(other: Context): Option[IList[(Term, Term)]] =
    Some(IList((context, other.context :/ Var(gap) / other.gap)))

  override def zip(other: Context): Option[IList[(Term, Term)]] =
    Some(IList((context, other.context)))

  override def mapImmediateSubtermsWithBindings(f: (ISet[Name], Term) => Term): Context =
    copy(context = f(ISet.empty, context))

  override def couplingRule(other: Context): Boolean =
    context embedsInto other.context

  override def freshen: Context =
    copy(context = context.freshen)

  override def arbitraryOrderingNumber: Int = 1
}

object C {
  def apply(buildContext: Name => Term): Context = {
    val gap = Name.fresh("_")
    Context(gap, buildContext(gap))
  }
}
