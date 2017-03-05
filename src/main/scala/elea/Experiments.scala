package elea

import elea.rewrite.Simplifier

object Experiments {
  def main(args: Array[String]): Unit = {
    implicit val program = Program.prelude
    val scc = Simplifier.supercompilation

    val t = lterm"(match (isSorted xs) (True -> isSorted (insert n xs)))"
    println(scc.run(t))
  }
}
