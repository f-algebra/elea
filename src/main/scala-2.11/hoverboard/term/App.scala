package hoverboard.term

import hoverboard.Name
import hoverboard.rewrite.Env

import scalaz.Scalaz._
import scalaz.{Name => _, _}

case class App private(fun: Term, args: NonEmptyList[Term]) extends Term with FirstOrder[Term] {
  require(!fun.isInstanceOf[App])

  override def apply(args2: IList[Term]) = App(fun, args :::> args2)

  override def driveHead(env: Env): Term =
    fun.driveHeadApp(env, args)

  override def driveHeadApp(env: Env, args2: NonEmptyList[Term]): Term =
    apply(args2.list)

  override def driveHeadCase(env: Env, enclosingCase: Case): Term =
    fun match {
      case fun: Constructor => fun.reduceCase(args.list, enclosingCase.branches).drive(env)
      case _ => super.driveHeadCase(env, enclosingCase)
    }

  private def flatten: App =
    fun match {
      case fun: App =>
        App(fun.fun, fun.args append args)
      case _ =>
        this
    }

  def mapImmediateSubtermsWithBindings(f: (ISet[Name], Term) => Term): Term =
    App(f(ISet.empty, fun), args.map(t => f(ISet.empty, t)).list)

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

  override def unfold: Term =
    fun.unfold.betaReduce(args)

  def isFPPF: Boolean =
    fun match {
      case fun: Fix => fun.isFPPF(args.list)
      case _ => false
    }
}

object App {
  def apply(fun: Term, args: IList[Term]): Term =
    fun match {
      case fun: App =>
        fun.apply(args)
      case _ =>
        args.toNel
          .map(App(fun, _))
          .getOrElse(fun)
    }
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
