package hoverboard

import hoverboard.term.Term
import org.scalatest.enablers.{Emptiness, Containing}

import scala.concurrent._
import scalaz.ISet

object Util {

  implicit object ContainsTerm extends Containing[ISet[Term]] {
    def contains(container: ISet[Term], element: Any): Boolean =
      element match {
        case e: Term => container.contains(e)
        case _ => false
      }

    def containsOneOf(container: ISet[Term], elements: Seq[Any]): Boolean =
      elements.exists(e => contains(container, e))

    def containsNoneOf(container: ISet[Term], elements: Seq[Any]): Boolean =
      elements.forall(e => !contains(container, e))
  }

  implicit def emptyISet[A] = new Emptiness[ISet[A]] {
    def isEmpty(thing: ISet[A]): Boolean = thing.isEmpty
  }
}

