package elea.term

import elea._
import elea.rewrite.Env

import scalaz.{Name => _, _}
import Scalaz._

case class Constructor(name: Name, argumentCount: Int, recursiveArgs: ISet[Int]) extends Atom {
  /**
    * Applies this constructor with the given arguments
    * to reduce a pattern match the the branches of the given case-of expression.
    */
  def reduceCase(args: IList[Term], branches: NonEmptyList[Branch]): Term =
    branches.findLeft(_.matches(this)) match {
      case None => Bot
      case Some(branch) =>
        branch.bindArgs(args)
    }

  override def reduceHeadCase(env: Env, enclosingCase: Case): Term =
    if (env.alreadySeen(enclosingCase)) {
      super.reduceHeadCase(env, enclosingCase)
    } else {
      reduceCase(IList.empty, enclosingCase.branches)
        .reduce(env.havingSeen(enclosingCase))
    }

  override def toString = Name.asDefinition(name.toString)

  def arbitraryOrderingNumber: Int = 4

  override def zip(other: Term) =
    other match {
      case other: Constructor if name == other.name =>
        Some(IList.empty)
      case _ =>
        None
    }

  override def order(other: Term) =
    other match {
      case other: Constructor => name ?|? other.name
      case _ => arbitraryOrderingNumber ?|? other.arbitraryOrderingNumber
    }
}
