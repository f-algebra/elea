package hoverboard.term

import hoverboard.Name

import scalaz.Ordering.EQ
import scalaz._
import Scalaz._

/**
  * Common functionality between [[Term]]s and things which contain them,
  * e.g. [[hoverboard.term.Branch]]
  */
abstract class TermLike[This <: TermLike[This]] {
  lazy val freeVars: ISet[Name] = getFreeVars

  protected def getFreeVars: ISet[Name] =
    ISet.unions(immediateSubtermsWithBindings.map { case (bs, t) => t.freeVars.difference(bs) }.toList)

  lazy val indices: ISet[Fix.Index] = getIndices

  protected def getIndices: ISet[Fix.Index] =
    ISet.unions(immediateSubterms.map(_.indices).toList)

  /**
    * Capture avoiding substitution
    */
  def :/(substitution: Substitution): This

  /**
    * Finds a substitution will yield `to` when applied to this term
    */
  def unifyLeft(to: This): Option[Substitution]

  /**
    * All of the sub-terms directly contained in `This`.
    * Also gives the bindings which have been captured for these sub-terms within `This`.
    */
  final def immediateSubtermsWithBindings: IList[(ISet[Name], Term)] = {
    // I'm going to functional programmer hell for this implementation
    var visited = IList.empty[(ISet[Name], Term)]
    mapImmediateSubtermsWithBindings { (names, term) =>
      visited = (names, term) :: visited
      term
    }
    visited
  }

  final def immediateSubterms: IList[Term] = immediateSubtermsWithBindings.map(_._2)

  final def subterms: IList[Term] = immediateSubterms.flatMap(t => t +: t.subterms)

  final def subtermsWithBindings: IList[(ISet[Name], Term)] = {
    var visited = IList.empty[(ISet[Name], Term)]
    mapSubtermsWithBindings { (bindings, term) =>
      visited = (bindings, term) :: visited
      term
    }
    visited
  }

  /**
    * Map a function over the sub-terms directly contained in `This`. This function is
    * also provided the set of variables which have been captured by `This`.
    * Must only touch every sub-term exactly once, as `f` may have side-effects.
    */
  def mapImmediateSubtermsWithBindings(f: (ISet[Name], Term) => Term): This

  final def mapSubtermsWithBindings(f: (ISet[Name], Term) => Term): This =
    mapImmediateSubtermsWithBindings { (bindings, term) =>
      def f2(bindings2: ISet[Name], term2: Term) = f(bindings.union(bindings2), term2)
      f(bindings, term.mapSubtermsWithBindings(f2))
    }

  final def mapImmediateSubterms(f: Term => Term): This =
    mapImmediateSubtermsWithBindings((_, t) => f(t))

  final def mapSubtermsContaining(vars: ISet[Name])(f: Term => Term): This =
    mapImmediateSubtermsWithBindings { case (bindings, term) =>
      if (bindings.intersection(vars).isEmpty)
        f(term.mapSubtermsContaining(vars)(f))
      else
        term
    }

  final def mapSubterms(f: Term => Term): This =
    mapImmediateSubterms(t => f(t.mapSubterms(f)))

  /**
    * All sub-terms containing all of the provided variables
    */
  final def subtermsContaining(vars: ISet[Name]): ISet[Term] =
    ISet.unions(immediateSubtermsWithBindings.map { case (bindings, subterm) =>
      if (vars.intersection(bindings).isEmpty &&
          vars.all(subterm.freeVars.contains)) {
        subterm.subtermsContaining(vars).insert(subterm)
      } else {
        ISet.empty[Term]
      }
    }.toList)

  final def subtermsContaining(vars: Name*): ISet[Term] =
    subtermsContaining(ISet.fromList(vars.toList))

  /**
    * Alpha equality
    */
  def =@=(other: This): Boolean =
    this unifyLeft other match {
      case Some(sub) => sub.isEmpty
      case None => false
    }

  /**
    * If two term-like things are of the same shape, zip up their matching immediate sub-terms.
    * [[None]] if they are not the same shape.
    */
  @inline
  def zip(other: This): Option[IList[(Term, Term)]]

  /**
    * Whether this term-like is the same shape as the `other` and the subterms of this
    * embed into the matching subterms of `other`.
    * The ''coupling'' rule of the homemorphic embedding.
    */
  def couplingRule(other: This): Boolean

  def order(other: This): Ordering

  /**
    * To make implementing [[scalaz.Order]] easier this is an arbitrary numeric representation of
    * what type of term this is, e.g. App is 0, Bot is 1, etc.
    */
  def arbitraryOrderingNumber: Int

  override def hashCode: Int =
    throw new IllegalAccessException("Don't use hash based collections for term-like things please")

  /**
    * Set all fix indices to [[Fix.Omega]]. Useful if you want to check equality modulo fix indices.
    * @return
    */
  def removeIndices: This =
    mapSubterms(_.removeIndices)
}
