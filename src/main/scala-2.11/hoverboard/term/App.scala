package hoverboard.term

import hoverboard.Name
import hoverboard.rewrite.Env

import scalaz.Scalaz._
import scalaz.{Name => _, _}

case class App(fun: Term, args: NonEmptyList[Term]) extends Term with FirstOrder[Term] {
  override def apply(args2: IList[Term]) = App(fun, args :::> args2)

  override def driveHead(env: Env): Term =
    fun.driveHeadApp(env, args)

  override def driveHeadApp(env: Env, args2: NonEmptyList[Term]): Term =
    App(fun, args.append(args2))

  override def driveHeadCase(env: Env, branches: NonEmptyList[Branch]): Term =
    fun match {
      case fun: Constructor => fun.reduceCase(args.list, branches).drive(env)
      case _ => super.driveHeadCase(env, branches)
    }

  def mapImmediateSubtermsWithBindings(f: (ISet[Name], Term) => Term): Term =
    App(f(ISet.empty, fun), args.map(t => f(ISet.empty, t)))

  override def toString = {
    def bracketIfNeeded(str: String) =
      if (str.contains(" ")) s"($str)" else str
    (fun <:: args).map(t => bracketIfNeeded(t.toString)).intercalate(" ")
  }

  def zip(other: Term): Option[IList[(Term, Term)]] =
    other match {
      case other: App if other.args.size == args.size =>
        Some((fun, other.fun) +: args.zip(other.args).list)
      case _ =>
        None
    }

  def arbitraryOrderingNumber: Int = 0

  override def order(other: Term): Ordering =
    other match {
      case other: App =>
        (args.size ?|? other.args.size) |+| args.fzipWith(other.args)(_ ?|? _).concatenate
      case _ =>
        arbitraryOrderingNumber ?|? other.arbitraryOrderingNumber
    }

  override def leftmost: Term = fun.leftmost

  override def deepBranches: IList[Term] =
    args.map(_.deepBranches).sequence.map(xs => App(fun, xs))

  /**
    * Is fixed-point promoted form
    */
  def isFPPF: Boolean =
    fun.isInstanceOf[Fix] &&
      args.all(_.isInstanceOf[Var]) &&
      args.distinct == args &&
      fun.freeVars.intersection(ISet.unions(args.map(_.freeVars).toList)).isEmpty
}

/**
  * View any [[Term]] as term application, potentially of zero arguments
  */
object AppView {
  def unapply(term: Term): Option[(Term, IList[Term])] =
    term match {
      case term: App => Some((term.fun, term.args.list))
      case _ => Some((term, IList.empty[Term]))
    }
}
