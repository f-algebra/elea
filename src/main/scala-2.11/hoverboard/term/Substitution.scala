package hoverboard.term

import hoverboard.Name

import scalaz._
import Scalaz._

case class Substitution private(val toMap: IMap[Name, Term]) {
  require(toMap.toList.all(m => Var(m._1) != m._2), "identity substitutions should be pre-filtered")

  lazy val freeVars = ISet.unions(toMap.values.map(_.freeVars))
  def boundVars = toMap.keySet

  /**
    * Equivalent to the identity substitution
    */
  def isEmpty = toMap.isEmpty

  /**
    * Remove a variable from this substitution
    * @return [[None]] if the result is empty
    */
  def -(name: Name): Substitution =
    this -- ISet.singleton(name)

  /**
    * The [[-]] operator applied multiple times
    */
  def --(names: ISet[Name]): Substitution = {
    val overlap = names.intersection(toMap.keySet)
    if (overlap.isEmpty)
      this
    else
      new Substitution(toMap.filterWithKey((name, _) => !overlap.contains(name)))
  }

  def +(elem: (Name, Term)): Option[Substitution] =
    toMap.lookup(elem._1) match {
      case None =>
        Some(new Substitution(toMap + elem))
      case Some(existingMatch) if existingMatch =@= elem._2 =>
        Some(this)
      case _ =>
        None
    }

  def ++(other: Substitution): Option[Substitution] =
    other.toMap.toList.foldLeft[Option[Substitution]](Some(this))((sub, elem) => sub.flatMap(_ + elem))

  def apply(term: Term): Term = term :/ this
}

object Substitution {
  def apply(mapping: (Name, Term)*): Substitution =
    new Substitution(IList(mapping : _*).filter(m => Var(m._1) != m._2).toMap)

  def zip(names: IList[Name], terms: IList[Term]): Substitution = {
    require(names.length == terms.length)
    Substitution(names.fzipWith[Term, (Name, Term)](terms)((_, _)).toList: _*)
  }

  def empty: Substitution =
    new Substitution(IMap.empty)

  def merge(subs: IList[Option[Substitution]]): Option[Substitution] =
    subs.foldLeft[Option[Substitution]](Some(Substitution.empty)) {
      case (Some(s1), Some(s2)) => s1 ++ s2
      case _ => None
    }

  def union(subs: IList[Substitution]): Option[Substitution] =
    merge(subs.map(Some(_)))
}
