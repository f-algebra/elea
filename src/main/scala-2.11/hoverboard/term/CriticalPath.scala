package hoverboard.term

sealed abstract class CriticalPath {
  def embedsInto(other: CriticalPath): Boolean
  def couplesWith(other: CriticalPath): Boolean
  def :/(sub: Substitution): CriticalPath
}

object CriticalPath {
  def invert(ending: Term, matches: Case.Index*): CriticalPath =
    matches.foldLeft(Terminal(ending): CriticalPath)((path, idx) => Match(idx, path))

  case class Match(index: Case.Index, subPath: CriticalPath) extends CriticalPath {
    override def couplesWith(other: CriticalPath): Boolean =
      other match {
        case other: Match =>
          index == other.index
        case _ =>
          false
      }

    override def embedsInto(other: CriticalPath): Boolean =
      other match {
        case other: Match if index == other.index =>
          subPath embedsInto other.subPath
        case other: Match =>
          this embedsInto other.subPath
        case _ =>
          false
      }

    override def :/(sub: Substitution): CriticalPath =
      copy(subPath = subPath :/ sub)
  }

  case class Terminal(term: Term) extends CriticalPath {
    override def couplesWith(other: CriticalPath): Boolean =
      false

    override def embedsInto(other: CriticalPath): Boolean =
      other match {
        case other: Match =>
          this embedsInto other.subPath
        case other: Terminal =>
          term embedsInto other.term
      }

    override def :/(sub: Substitution): CriticalPath =
      copy(term = term :/ sub)
  }
}
