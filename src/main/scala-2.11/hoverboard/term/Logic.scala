package hoverboard.term

import scalaz.{NonEmptyList, ISet}
import hoverboard._

object Logic {
  // For the parser
  private implicit val program: Program = Program.empty

  val Truth: Term = Bot
  val Falsity: Term = Constructor("ff", argumentCount = 0, recursiveArgs = ISet.empty)

  def equality(leftTerm: Term, rightTerm: Term): Term =
    and(leftTerm =< rightTerm, rightTerm =< leftTerm)

  def or(leftTerm: Term, rightTerm: Term): Term =
    term"case $leftTerm | else -> $rightTerm end"

  def not(propTerm: Term): Term =
    Falsity =< propTerm

  def and(leftTerm: Term, rightTerm: Term): Term =
    not(or(not(leftTerm), not(rightTerm)))
}
