package elea.rewrite

sealed abstract class Direction {
  def canIncrease: Boolean = false
  def canDecrease: Boolean = false
  def invert: Direction
}

object Direction {

  /** Rewriting to a potentially more defined term */
  case object Increasing extends Direction {
    override def canIncrease = true
    def invert = Decreasing
  }

  /** Rewriting to a potentially less defined term */
  case object Decreasing extends Direction {
    override def canDecrease = true
    def invert = Increasing
  }

  /** Rewriting which preserves equivalence */
  case object Equal extends Direction {
    def invert = Equal
  }

}
