package hoverboard.rewrite

import hoverboard.Name
import hoverboard.term.{Pattern, Term, UMap}

import scalaz._
import Scalaz._

case class Env(rewriteDirection: Direction,
               matches: UMap[Term, Pattern],
               history: IList[Term]) {
  def invertDirection: Env =
    copy(rewriteDirection = rewriteDirection.invert)

  def withMatch(term: Term, pattern: Pattern) =
    copy(matches = matches + (term -> pattern))

  def alreadySeen(term: Term) =
    history.any(_ embedsInto term)

  def havingSeen(term: Term): Env =
    copy(history = term :: history)

  def withBindings(bindings: ISet[Name]): Env =
    copy(matches = matches.filterKeys { t =>
      t.freeVars.intersection(bindings).isEmpty
    })

  def bindingsSet: ISet[Name] =
    ISet.unions(matches.toSeq.map(m => m._1.freeVars.union(m._2.bindingsSet)).toList)
}

object Env {
  val empty = Env(Direction.Increasing, UMap.empty, IList.empty)
}
