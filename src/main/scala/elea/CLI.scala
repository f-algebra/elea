package elea

import java.io.File

import elea.rewrite.Simplifier
import scopt.OptionParser

/**
  * Command-line interface main method
  */
object CLI {
  case class Config(
    recordStats: Boolean = false,
    fromFile: Option[File] = None)

  val programVersion = getClass.getPackage.getImplementationVersion

  val configParser = new OptionParser[Config]("elea") {
    head(s"elea $programVersion - a supercompiler for theorem provers")

    opt[Unit]('s', "stats")
      .action((_, config) => config.copy(recordStats = true))
      .text("print out performance statistics in comments")

    opt[File]('i', "input-file")
        .action((file, config) => config.copy(fromFile = Some(file)))
  }

  val supercompiler = Simplifier.supercompilation

  def main(args: Array[String]): Unit = {
    val input = io.Source.stdin.getLines.mkString
    val config = configParser
        .parse(args, Config())
        .getOrElse(throw new IllegalArgumentException("Bad command line parameters"))
    run(config, input)
  }

  def run(config: Config, inputProgram: String): Unit = {
    implicit val startingProgram = Program.empty
    Parser.parseAll(inputProgram) { termDef =>
      val startTime = System.nanoTime()
      val simplifiedDef = termDef.modifyTerm(supercompiler.run)
      val finishTime = System.nanoTime()

      println(simplifiedDef.toString)

      if (config.recordStats) {
        val timeTakenMillis = (finishTime - startTime) / (1000 * 1000)
        println(s";; took ${timeTakenMillis}ms")
      }

      simplifiedDef
    }
  }
}
