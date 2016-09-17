package hoverboard

import scalaz.Scalaz._
import scalaz._

package object term {

  import Fix._
  import Ordering._

  val Truth: Term = Bot
  val Falsity: Term = Constructor("ff", argumentCount = 0, recursiveArgs = ISet.empty)
  val ⊥ = Bot

  implicit def termLikeOrder[T <: TermLike[T]]: Order[T] =
    Order.order(_ order _)

  implicit val patternOrder: Order[Pattern] =
    Order.orderBy((x: Pattern) => (x.constructor.name, x.bindings))

  implicit def stringToCaseIndex(name: String): Case.Index = Case.Index(Name(name))
}
