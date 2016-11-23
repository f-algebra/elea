package hoverboard.term

import hoverboard.Name
import hoverboard.rewrite.Env

import scalaz.{Name => _, _}
import Scalaz._

case class Var(name: Name) extends Term {
  override def reduceHead(env: Env) =
    env.matches
      .lookup(this)
      .map(_.asTerm)
      .getOrElse(this)

  override lazy val freeVars = ISet.singleton(name)

  override def :/(sub: Substitution): Term =
    sub.toMap.lookup(name).getOrElse(this)

  override def unifyLeftUnchecked(to: Term): Option[Substitution] =
    Some(to / name)

  override def couplingRule(other: Term) = other.isInstanceOf[Var]

  override def mapImmediateSubtermsWithBindings(f: (ISet[Name], Term) => Term): Term = this

  override def toString = name.toString

  def zip(other: Term): Option[IList[(Term, Term)]] =
    other match {
      case other: Var if name == other.name =>
        Some(IList.empty)
      case _ =>
        None
    }

  override def uzip(other: Term): Option[IList[(Term, Term)]] =
    zip(other)

  def arbitraryOrderingNumber: Int = 7

  def order(other: Term): Ordering =
    other match {
      case other: Var => name ?|? other.name
      case _ => arbitraryOrderingNumber ?|? other.arbitraryOrderingNumber
    }

  override def freshen = this
}
