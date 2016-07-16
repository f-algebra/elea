package hoverboard.term

import hoverboard.Name
import hoverboard._
import hoverboard.rewrite.Env

import scalaz.{Name => _, _}
import Scalaz._

case class Lam(binding: Name, body: Term) extends Term {
  override def driveHead(env: Env): Term = this

  override def driveSubterms(env: Env): Term =
    avoidCapture(env.bindingsSet).mapImmediateSubterms(_.drive(env))

  override def :/(sub: Substitution): Lam = {
    val newSub = sub - binding
    if (newSub.isEmpty)
      this
    else
      avoidCapture(newSub.freeVars).mapImmediateSubterms(_ :/ newSub).asInstanceOf[Lam]
  }

  def unifyLeft(to: Term): Option[Substitution] =
    to match {
      case Lam(otherBinding, otherBody) =>
        body.unifyLeft(otherBody :/ (Var(binding) / otherBinding))
      case _ =>
        None
    }

  def couplingRule(other: Term) =
    other match {
      case other: Lam => body embedsInto other.body
      case _ => false
    }

  /**
    * Will freshen the bound variable of this lambda term if it clashes with the provided names
    */
  def avoidCapture(names: ISet[Name]): Lam =
    if (names.contains(binding)) {
      val fresh = binding.freshen
      Lam(fresh, body :/ (Var(fresh) / binding))
    }
    else this

  def mapImmediateSubtermsWithBindings(f: (ISet[Name], Term) => Term): Lam =
    Lam(binding, f(ISet.singleton(binding), body))

  override def betaReduce(args: NonEmptyList[Term]): Term = {
    val reduced = body :/ args.head / binding
    args.tail.toNel.fold(reduced)(args => reduced.betaReduce(args))
  }

  override def driveHeadApp(env: Env, args: NonEmptyList[Term]): Term =
    betaReduce(args).drive(env.havingSeen(App(this, args)))

  override def driveHeadCase(env: Env, enclosingCase: Case): Term =
    throw new IllegalStateException("Cannot pattern match a lambda")

  override def flattenLam: (IList[Name], Term) =
    first(flatten)(_.list)

  def flatten: (NonEmptyList[Name], Term) = {
    val (bindings, innerBody) = body.flattenLam
    (NonEmptyList.nel(binding, bindings), innerBody)
  }

  override def toString = {
    val (bindings, body) = flattenLam
    "fn " + bindings.map(_.toString + " ").concatenate +
      "-> " + body.toString
  }

  def arbitraryOrderingNumber: Int = 5

  def zip(other: Term): Option[IList[(Term, Term)]] =
    other match {
      case other: Lam if binding == other.binding =>
        Some(IList((body, other.body)))
      case _ =>
        None
    }

  override def uzip(other: Term): Option[IList[(Term, Term)]] =
    other match {
      case other: Lam =>
        Some(IList((body, other.body :/ (Var(binding) / other.binding))))
      case _ =>
        None
    }

  def order(other: Term): Ordering =
    other match {
      case other: Lam =>
        (binding ?|? other.binding) |+| (body ?|? other.body)
      case _ =>
        arbitraryOrderingNumber ?|? other.arbitraryOrderingNumber
    }

  override def freshen: Term =
    avoidCapture(ISet.singleton(binding)).mapImmediateSubterms(_.freshen)

  override def unfold: Term =
    copy(body = body.unfold)
}

object Lam {
  def apply(bindings: IList[Name], body: Term): Term =
    bindings match {
      case INil() => body
      case ICons(head, tail) => Lam(head, Lam(tail, body))
    }

  def apply(bindings: NonEmptyList[Name], body: Term): Lam =
    Lam(bindings.head, Lam(bindings.tail, body))
}
