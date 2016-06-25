package hoverboard

import hoverboard.term.Fix.Index
import org.scalactic.Equality

import scalaz._
import Scalaz._

package object term {

  import Fix._
  import Ordering._

  val Truth: Term = Bot
  val Falsity: Term = Constructor("ff", argumentCount = 0, recursiveArgs = ISet.empty)

  implicit def termLikeOrder[T <: TermLike[T]]: Order[T] =
    Order.order(_ order _)

  implicit val patternOrder: Order[Pattern] =
    Order.orderBy((x: Pattern) => (x.constructor.name, x.bindings))

  implicit val fixIndexOrder: Order[Fix.Index] = (x: Index, y: Index) => (x, y) match {
    case (Omega, Omega) => EQ
    case (Omega, _) => GT
    case (_, Omega) => LT
    case (Finite(n), Finite(m)) => n ?|? m
  }

  /**
    * A [[Context]] is a term with a gap denoted as a "_" variable
    */
  type Context = Term
  val contextGap: Name = "_"
}
