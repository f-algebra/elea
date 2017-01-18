package elea.term

import elea.Name

import scalaz.{Name => _, _}
import Scalaz._

/**
  * Terms without sub-terms or variables
  */
abstract class Atom extends Term with FirstOrder[Term] {
  def mapImmediateSubtermsWithBindings(f: (ISet[Name], Term) => Term): Term = this

  def zip(other: Term) =
    if (this == other) Some(IList.empty) else None

  def order(other: Term) =
    arbitraryOrderingNumber ?|? other.arbitraryOrderingNumber
}
