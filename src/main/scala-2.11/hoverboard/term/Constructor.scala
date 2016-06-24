package hoverboard.term

import hoverboard.Name
import hoverboard.rewrite.Env

import scalaz.{Name => _, _}
import Scalaz._

case class Constructor(name: Name, argumentCount: Int, recursiveArgs: ISet[Int]) extends Atom {
  /**
    * Applies this constructor with the given arguments
    * to reduce a pattern match with the given branches
    */
  def reduceCase(args: IList[Term], caseBranches: NonEmptyList[Branch]): Term =
    caseBranches.findLeft(_.matches(this)) match {
      case None => Bot
      case Some(branch) =>
        branch.bindArgs(args)
    }

  override def driveHeadCase(env: Env, branches: NonEmptyList[Branch]): Term =
    reduceCase(IList.empty, branches).drive(env)

  override def toString = name.toString

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
