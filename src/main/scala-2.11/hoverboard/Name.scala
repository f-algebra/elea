package hoverboard

import scala.collection.mutable

/**
  * A variable name
  * @param freshener Something to distinguish identically named variables
  */
case class Name private(name: String, freshener: Option[Int]) {
  def freshen: Name = {
    assert(freshener.isEmpty || Name.latestFresheners.contains(name))
    val latestFreshener = Name.latestFresheners.getOrElse(name, 0)
    Name.latestFresheners.put(name, latestFreshener + 1)
    Name(name, Some(latestFreshener))
  }

  override def toString =
    name + freshener.fold("")("[" + _ + "]")
}

object Name {
  /**
    * Keep track of the identifiers we have used to freshen variables in the past.
    * Used by [[Name.freshen()]].
    */
  private val latestFresheners = new mutable.HashMap[Name, Int]()

  def apply(name: String): Name = Name(name, None)

  implicit def stringToName(name: String): Name = Name(name)

  def freshIndex: Name = Name("α").freshen
}
