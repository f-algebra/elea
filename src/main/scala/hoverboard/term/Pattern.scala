package hoverboard.term

import hoverboard.Name

import scalaz.{Name => _, _}
import Scalaz._

case class Pattern(constructor: Constructor, bindings: IList[Name]) {
  val bindingsSet: ISet[Name] = ISet.fromFoldable(bindings)
  require(bindingsSet.size == bindings.length, s"Duplicate binding in $this")

  def replaceBindings(sub: Name ==>> Name) =
    Pattern(constructor, bindings.map(n => sub.lookup(n).getOrElse(n)))

  def asTerm: Term =
    constructor.apply(bindings.map[Term](Var))

  override def toString =
    constructor.toString +
      bindings.map(" " + _.toString).concatenate
}

object Pattern {
  def from(term: Term) =
    term match {
      case AppView(con: Constructor, args: IList[Term]) if args.all(_.isInstanceOf[Var]) =>
        Pattern(con, args.map(_.asInstanceOf[Var].name))
      case _ =>
        throw new AssertionError(s"Cannot match term to a pattern: $term")
    }
}