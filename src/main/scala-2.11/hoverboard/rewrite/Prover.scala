package hoverboard.rewrite

class Prover(simplifier: => Simplifier) {
  /**
    * Check if the set of pattern matches in this environment is unsatisfiable,
    * aka this branch is unreachable
    */
  def unsatisfiable(env: Env): Boolean = {
    ???
  }
}
