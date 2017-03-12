package elea.term

import elea._
import elea.rewrite.Env

import scalaz.{Name => _, _}
import Scalaz._

case class Lam(binding: Name, body: Term) extends Term {
  override def reduceHead(env: Env): Term =
    if (body == Bot)
      Bot
    else
      this

  override def reduceSubterms(env: Env): Term =
    avoidCapture(env.bindingsSet).mapImmediateSubterms(_.reduce(env))

  override def :/(sub: Substitution): Lam = {
    val newSub = sub - binding
    if (newSub.isEmpty)
      this
    else
      avoidCapture(newSub.freeVars).mapImmediateSubterms(_ :/ newSub).asInstanceOf[Lam]
  }

  def unifyLeftUnchecked(to: Term): Option[Substitution] =
    to match {
      case Lam(otherBinding, otherBody) =>
        (body unifyLeftUnchecked (otherBody :/ (Var(binding) / otherBinding)))
          .filterNot(_.boundVars.contains(binding))
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

  override def reduceHeadApp(env: Env, args: NonEmptyList[Term]): Term =
    betaReduce(args).reduce(env.havingSeen(App(this, args)))

  override def reduceHeadCase(env: Env, enclosingCase: Case): Term =
    throw new IllegalStateException("Cannot pattern match a lambda")

  override def flattenLam: (IList[Name], Term) =
    first(flatten)(_.list)

  def flatten: (NonEmptyList[Name], Term) = {
    val (bindings, innerBody) = body.flattenLam
    (NonEmptyList.nel(binding, bindings), innerBody)
  }

  override def toLisp(settings: LispPrintSettings) = {
    val (bindings, body) = flattenLam
    s"(fun ${bindings.toList.map(_.toString).mkString(" ")} ${body.toLisp(settings)})"
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
