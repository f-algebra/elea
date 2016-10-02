package hoverboard.term

import hoverboard._
import hoverboard.rewrite.Env

import scalaz.{Name => _, _}
import Scalaz._

/**
  * A branch of a case-of expression
  */
sealed abstract class Branch extends TermLike[Branch] {
  val body: Term

  def reduce(env: Env, matchedTerm: Term): Branch

  /**
    * Whether this branch always returns the same term that was being matched.
    */
  def isIdentity(matchedTerm: Term): Boolean

  /**
    * Does this branch match to the given constructor
    */
  def matches(constructor: Constructor): Boolean

  /**
    * Given a list of arguments to a matching constructor, return the `body` of this branch
    * with these arguments applied as a substitution to the pattern variables being matched.
    */
  def bindArgs(constructorArgs: IList[Term]): Term

  /**
    * Will freshen any bound variables of this branch which clash with the provided names
    */
  def avoidCapture(names: ISet[Name]): Branch

  /**
    * The set of variables captured by the pattern of this branch.
    */
  def capturedVars: ISet[Name]

  override def equals(other: Any): Boolean =
    other match {
      case other: Branch => order(other) == Ordering.EQ
      case _ => false
    }
}

case class DefaultBranch(body: Term) extends Branch with FirstOrder[Branch] {
  def reduce(env: Env, matchedTerm: Term) = DefaultBranch(body.reduce(env))

  def bindArgs(constructorArgs: IList[Term]): Term = body

  def mapImmediateSubtermsWithBindings(f: (ISet[Name], Term) => Term): Branch =
    DefaultBranch(f(ISet.empty, body))

  def isIdentity(matchedTerm: Term): Boolean = body == matchedTerm

  def matches(con: Constructor) = true

  def avoidCapture(names: ISet[Name]): DefaultBranch = this

  def capturedVars = ISet.empty[Name]

  override def toString =
    "| else -> " + body.toString

  def zip(other: Branch): Option[IList[(Term, Term)]] =
    other match {
      case other: DefaultBranch => Some(IList.empty)
      case _ => None
    }

  def arbitraryOrderingNumber: Int = 0

  def order(other: Branch): Ordering =
    arbitraryOrderingNumber ?|? other.arbitraryOrderingNumber

  def explore: IList[Branch] = for {
    newBody <- body.explore
  } yield DefaultBranch(newBody)
}

case class PatternBranch(pattern: Pattern, body: Term) extends Branch {
  def reduce(env: Env, matchedTerm: Term) = {
    val branch = avoidCapture(env.bindingsSet.union(matchedTerm.freeVars))
    branch.copy(body = branch.body.reduce(env.withMatch(matchedTerm, branch.pattern)))
  }

  def bindArgs(constructorArgs: IList[Term]): Term = {
    require(pattern.bindings.length == constructorArgs.length,
      "Cannot bind a pattern match with a different number of arguments. " +
        s"Attempted to bind $constructorArgs to ${pattern.bindings}")
    body :/ Substitution.zip(pattern.bindings, constructorArgs)
  }

  def :/(sub: Substitution): Branch = {
    val newSub = sub -- pattern.bindingsSet
    if (newSub.isEmpty)
      this
    else {
      val overlap = pattern.bindingsSet.intersection(newSub.freeVars)
      if (overlap.isEmpty) {
        PatternBranch(pattern, body :/ newSub)
      } else {
        val freshBranch = avoidCapture(overlap)
        freshBranch.copy(body = freshBranch.body :/ newSub)
      }
    }
  }

  def unifyLeft(to: Branch): Option[Substitution] =
    to match {
      case to: PatternBranch
          if pattern.constructor == to.pattern.constructor &&
            pattern.bindings.length == to.pattern.bindings.length  =>
        val bindingSub = Substitution.zip(to.pattern.bindings, pattern.bindings.map(Var))
        body.unifyLeft(to.body :/ bindingSub)
      case _ => None
    }

  def couplingRule(other: Branch): Boolean =
    other match {
      case other: PatternBranch if pattern.constructor == other.pattern.constructor =>
         body embedsInto other.body
      case _ => false
    }

  def mapImmediateSubtermsWithBindings(f: (ISet[Name], Term) => Term): Branch =
    PatternBranch(pattern, f(pattern.bindingsSet, body))

  def isIdentity(matchedTerm: Term): Boolean =
    body == pattern.asTerm || body == matchedTerm

  def matches(constructor: Constructor) =
    pattern.constructor == constructor

  def avoidCapture(names: ISet[Name]): PatternBranch = {
    val needFreshening = names.intersection(capturedVars).toIList
    if (needFreshening.isEmpty) {
      this
    } else {
      val freshened = needFreshening.map(_.freshen)
      val freshPattern = pattern.replaceBindings(needFreshening.zip(freshened).toMap)
      val freshBody = body :/ Substitution.zip(needFreshening, freshened.map(Var.apply))
      PatternBranch(freshPattern, freshBody)
    }
  }

  def capturedVars = pattern.bindingsSet

  override def toString =
    "| " + pattern.toString + " -> " + body.toString

  def zip(other: Branch): Option[IList[(Term, Term)]] =
    other match {
      case other: PatternBranch if pattern == other.pattern =>
        Some(IList((body, other.body)))
      case _ =>
        None
    }

  override def uzip(other: Branch): Option[IList[(Term, Term)]] =
    other match {
      case other: PatternBranch =>
        Some(IList((body, other.body :/ Substitution.zip(other.pattern.bindings, pattern.bindings.map(Var)))))
      case _ =>
        None
    }

  def arbitraryOrderingNumber: Int = 1

  def order(other: Branch): Ordering =
    other match {
      case other: PatternBranch =>
        (pattern ?|? other.pattern) |+| (body ?|? other.body)
      case _ =>
        arbitraryOrderingNumber ?|? other.arbitraryOrderingNumber
    }

  def explore: IList[Branch] = for {
    newBody <- body.explore
  } yield PatternBranch(pattern, newBody)

  override def freshen: Branch =
    avoidCapture(pattern.bindingsSet).mapImmediateSubterms(_.freshen)
}
