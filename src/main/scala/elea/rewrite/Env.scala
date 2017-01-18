package elea.rewrite

import elea.term._
import elea.Name
import elea.rewrite.Supercompiler.Fold

import scalaz.{Name => _, _}
import Scalaz._

case class Env(rewriteDirection: Direction,
               matches: UMap[Term, Pattern],
               termHistory: IList[Term],
               foldHistory: IList[Fold]) {
  def invertDirection: Env =
    copy(rewriteDirection = rewriteDirection.invert)

  def withMatch(term: Term, pattern: Pattern) =
    if (term.isInstanceOf[Case] ||              // Not worth keeping pattern matches that will just be reduced
      term.leftmost.isInstanceOf[Constructor])
      this
    else {
      val newMatches =
        term match {
          case x: Var =>
            matches :/ (pattern.asTerm / x.name)  // Apply the pattern match as a rewrite to existing stored matches
          case _ =>
            matches
        }

      copy(matches = newMatches + (term -> pattern))
    }

  def alreadySeen(term: Term) =
    termHistory.any(_ embedsInto term)

  def alreadySeen(fold: Fold) =
    foldHistory.any(_ embedsInto fold)

  def havingSeen(term: Term): Env =
    copy(termHistory = term :: termHistory)

  def havingSeen(fold: Fold): Env =
    copy(foldHistory = fold :: foldHistory)

  def withBindings(bindings: ISet[Name]): Env =
    copy(matches = matches.filterKeys { t =>
      t.freeVars.intersection(bindings).isEmpty
    })

  def clearHistory: Env =
    copy(termHistory = IList.empty, foldHistory = IList.empty)

  def clearMatches: Env =
    copy(matches = UMap.empty)

  def bindingsSet: ISet[Name] =
    ISet.unions(matches.toSeq.map(m => m._1.freeVars.union(m._2.bindingsSet)).toList)
}

object Env {
  val empty = Env(Direction.Increasing, UMap.empty, IList.empty, IList.empty)
}
