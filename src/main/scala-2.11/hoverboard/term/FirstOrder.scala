package hoverboard.term

import scalaz.Scalaz._
import scalaz._

/**
  * Provides method implementations for [[TermLike]] things which do not bind any variables
  */
trait FirstOrder[This <: TermLike[This]] extends TermLike[This] {
  def :/(sub: Substitution): This =
    mapImmediateSubterms(_ :/ sub)

  def unifyLeft(to: This): Option[Substitution] =
    zip(to).flatMap(zipped => Substitution.merge(zipped.map(z => z._1.unifyLeft(z._2))))

  def couplingRule(other: This): Boolean =
    zip(other) match {
      case None => false
      case Some(zipped) =>
        zipped.all(z => z._1.embedsInto(z._2))
    }
}
