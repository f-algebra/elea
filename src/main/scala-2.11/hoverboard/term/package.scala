package hoverboard

import org.scalactic.Equality

import scalaz._
import Scalaz._

package object term {

  val Truth: Term = Bot
  val Falsity: Term = Constructor("ff", argumentCount = 0, recursiveArgs = ISet.empty)

  implicit object TermAlphaEq extends Equality[Term] {
    override def areEqual(a: Term, b: Any): Boolean =
      b match {
        case b: Term => a =@= b
        case _ => false
      }
  }

  implicit def termLikeOrder[T <: TermLike[T]]: Order[T] =
    Order.order(_ order _)

  implicit val patternOrder: Order[Pattern] =
    Order.orderBy((x: Pattern) => (x.constructor.name, x.bindings))

  /**
    * A [[Context]] is a term with a gap denoted as a "_" variable
    */
  type Context = Term
  val contextGap: Name = "_"
}
