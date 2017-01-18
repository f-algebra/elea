package elea

import scalaz.Scalaz._
import scalaz._

package object term {

  import Fix._
  import Ordering._

  val ⊥ = Bot

  implicit def termLikeOrder[T <: TermLike[T]]: Order[T] =
    Order.order(_ order _)

  implicit val patternOrder: Order[Pattern] =
    Order.orderBy((x: Pattern) => (x.constructor.name, x.bindings))

  implicit val caseIndexOrder: Order[Case.Index] =
    Order.orderBy { case Case.Index.Named(name) => name }

  implicit def stringToCaseIndex(name: String): Case.Index = Case.Index.fromName(Name(name))


}
