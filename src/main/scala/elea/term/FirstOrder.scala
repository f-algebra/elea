package elea.term

import scalaz.Scalaz._
import scalaz._

/**
  * Provides method implementations for [[TermLike]] things which do not bind any variables
  */
trait FirstOrder[This <: TermLike[This]] extends TermLike[This] {
  override def :/(sub: Substitution): This =
    mapImmediateSubterms(_ :/ sub)

  override def unifyLeftUnchecked(to: This): Option[Substitution] =
    zip(to).flatMap(zipped => Substitution.merge(zipped.map(z => z._1.unifyLeftUnchecked(z._2))))

  override def couplingRule(other: This): Boolean =
    zip(other) match {
      case None => false
      case Some(zipped) =>
        zipped.all(z => z._1.embedsInto(z._2))
    }

  override def uzip(other: This): Option[IList[(Term, Term)]] =
    zip(other)

  override def freshen: This =
    mapImmediateSubterms(_.freshen)
}
