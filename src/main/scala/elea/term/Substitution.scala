package elea.term

import elea.Name

import scalaz.{Name => _, _}
import Scalaz._

class Substitution private (mapping: IMap[Name, Term]) extends FirstOrder[Substitution] {

  val toMap: IMap[Name, Term] =
    mapping.filterWithKey { (name, term) => Var(name) != term }

  override lazy val freeVars = ISet.unions(toMap.values.map(_.freeVars))

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
      case _ if Var(name) == term =>
        Some(this)
      case None =>
        Some(new Substitution(toMap + (name -> this.apply(term))))
      case Some(existingMatch) if existingMatch =@= term =>
        Some(this)
      case _ =>
        None
    }
  }

  /**
    * Add a mapping to this substitution for a variable which you know is not already mapped by the substitution.
    * For example, a mapping from a fresh variable.
    */
  def +!(elem: (Name, Term)): Substitution = {
    require(!boundVars.contains(elem._1), s"${elem._1} already exists in substitution $this")
    (this + elem).get
  }

  def ++!(other: Substitution): Substitution = {
    other.toMap.toList.foldLeft[Substitution](this)((sub, elem) => sub +! elem)
  }

  def ++(other: Substitution): Option[Substitution] =
    other.toMap.toList.foldLeft[Option[Substitution]](Some(this))((sub, elem) => sub.flatMap(_ + elem))

  def apply(term: Term): Term = term :/ this

  def size: Int = toMap.size

  override def mapImmediateSubtermsWithBindings(f: (ISet[Name], Term) => Term): Substitution =
    new Substitution(toMap.map(f(ISet.empty, _)))

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

  override def hashCode: Int = toMap.hashCode

  override def equals(obj: Any): Boolean =
    obj match {
      case obj: Substitution => toMap == obj.toMap
      case _ => false
    }
}

object Substitution {
  def apply(mapping: (Name, Term)*): Substitution =
    new Substitution(IList(mapping : _*).toMap)

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

  def unionDisjoint(subs: IList[Substitution]): Substitution =
    union(subs).getOrElse {
      throw new IllegalArgumentException(s"Tried to union overlapping substitutions: $subs")
    }
}
