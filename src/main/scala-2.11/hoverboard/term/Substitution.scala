package hoverboard.term

import hoverboard.Name

import scalaz._
import Scalaz._

case class Substitution private (toMap: IMap[Name, Term]) extends FirstOrder[Substitution] {
  require(toMap.toList.all(m => Var(m._1) != m._2), "identity substitutions should be pre-filtered")

  override protected def getFreeVars = ISet.unions(toMap.values.map(_.freeVars))

  def boundVars = toMap.keySet

  /**
    * Equivalent to the identity substitution
    */
  def isEmpty = toMap.isEmpty

  def nonEmpty = !isEmpty

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

  def +(elem: (Name, Term)): Option[Substitution] = {
    val (name, term) = elem
    toMap.lookup(name) match {
      case None =>
        Some(new Substitution(toMap + (name -> this.apply(term))))
      case Some(existingMatch) if existingMatch =@= term =>
        Some(this)
      case _ =>
        None
    }
  }

  def ++(other: Substitution): Option[Substitution] =
    other.toMap.toList.foldLeft[Option[Substitution]](Some(this))((sub, elem) => sub.flatMap(_ + elem))

  def apply(term: Term): Term = term :/ this

  override def mapImmediateSubtermsWithBindings(f: (ISet[Name], Term) => Term): Substitution =
    copy(toMap = toMap.map(f(ISet.empty, _)))

  override def order(other: Substitution): Ordering =
    toMap ?|? other.toMap

  override def zip(other: Substitution): Option[IList[(Term, Term)]] =
    if (toMap.keySet == other.toMap.keySet) {
      Some(toMap.intersectionWith(other.toMap)((_, _)).values.toIList)
    } else {
      None
    }

  override def arbitraryOrderingNumber: Int = 1

  override def toString =
    toMap.toList.map { case (x, t) => s"$x -> $t" } .mkString("\n")
}

object Substitution {
  def apply(mapping: (Name, Term)*): Substitution =
    new Substitution(IList(mapping : _*).filter(m => Var(m._1) != m._2).toMap)

  def zip(names: IList[Name], terms: IList[Term]): Substitution = {
    require(names.length == terms.length)
    Substitution(names.fzipWith[Term, (Name, Term)](terms)((_, _)).toList: _*)
  }

  def fromMap(map: IMap[Name, Term]): Substitution =
    new Substitution(map)

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
