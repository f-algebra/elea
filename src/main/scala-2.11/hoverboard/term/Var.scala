package hoverboard.term

import hoverboard.Name
import hoverboard.rewrite.Env

import scalaz._
import Scalaz._

case class Var(name: Name) extends Term {
  override def driveHead(env: Env) =
    env.matches.lookup(this).fold(this: Term)(_.asTerm)

  protected override def getFreeVars = ISet.singleton(name)

  def :/(sub: Substitution): Term =
    sub.toMap.lookup(name).getOrElse(this)

  def unifyLeft(to: Term): Option[Substitution] =
    Some(to / name)

  def couplingRule(other: Term) = other.isInstanceOf[Var]

  def mapImmediateSubtermsWithBindings(f: (ISet[Name], Term) => Term): Term = this

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
