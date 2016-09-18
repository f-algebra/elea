package hoverboard.term

import scalaz.{IList, NonEmptyList, ISet}

object Logic {
  val Truth: Term = Bot
  val Falsity: Constructor = new Constructor("false", argumentCount = 0, recursiveArgs = ISet.empty) {
    override def toString = "false"
  }

  def equality(leftTerm: Term, rightTerm: Term): Term =
    and(leftTerm =< rightTerm, rightTerm =< leftTerm)

  def or(leftTerm: Term, rightTerm: Term): Term =
    Case(leftTerm, NonEmptyList(PatternBranch(Pattern(Falsity, IList.empty), rightTerm)), Case.freshIndex)

  def not(propTerm: Term): Term =
    Falsity =< propTerm

  def and(leftTerm: Term, rightTerm: Term): Term =
    not(or(not(leftTerm), not(rightTerm)))

  def and(terms: IList[Term]): Term =
    terms.foldLeft(Truth)(and)

  def or(terms: IList[Term]): Term =
    terms.foldLeft(Falsity: Term)(or)
}
