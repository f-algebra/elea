package hoverboard.term

import hoverboard.rewrite.Env
import hoverboard.Name

import scalaz.{Name => _, _}
import Scalaz._

/**
  * Denotational less-defined-or-equal-to.
  * Acts as reverse implication for properties, since _|_ denotes truth.
  */
case class Leq(smallerTerm: Term, largerTerm: Term)
  extends Term with FirstOrder[Term] {

  override def driveHead(env: Env): Term =
    if (smallerTerm == Bot)
      Logic.Truth
    else if (largerTerm == Logic.Falsity)
      Logic.Truth
    else if (smallerTerm =@= largerTerm)
      Logic.Truth
    else smallerTerm match {
      case smallerTerm: Case =>
        C(x => Leq(Var(x), largerTerm))
          .applyToBranches(smallerTerm)
          .driveIgnoringMatchedTerm(env)
      case AppView(smallerConFun: Constructor, smallerConArgs) =>
        if (largerTerm == Bot)
          Logic.Falsity
        else largerTerm match {
          case AppView(largerConFun: Constructor, largerConArgs) =>
            if (largerConFun.name != smallerConFun.name)
              Logic.Falsity
            else
              Logic.and(smallerConArgs.fzipWith(largerConArgs)(Leq))
                .drive(env)
          case Leq(ff, prop) if ff == Logic.Falsity && smallerConFun == Logic.Falsity =>
            // Double negation elimination
            prop
          case _ =>
            this
        }
      case _ =>
        this
    }

  override def driveSubterms(env: Env): Term = {
    Leq(smallerTerm.drive(env), largerTerm.drive(env.invertDirection))
  }

  def mapImmediateSubtermsWithBindings(f: (ISet[Name], Term) => Term): Term =
    Leq(f(ISet.empty[Name], smallerTerm), f(ISet.empty[Name], largerTerm))

  override def toString = s"$smallerTerm =< $largerTerm"

  def arbitraryOrderingNumber: Int = 6

  def zip(other: Term): Option[IList[(Term, Term)]] =
    other match {
      case other: Leq =>
        Some(IList((smallerTerm, other.smallerTerm), (largerTerm, other.largerTerm)))
      case _ =>
        None
    }

  def order(other: Term): Ordering =
    other match {
      case other: Leq =>
        (smallerTerm ?|? other.smallerTerm) |+| (largerTerm ?|? other.largerTerm)
      case _ =>
        arbitraryOrderingNumber ?|? other.arbitraryOrderingNumber
    }
}
