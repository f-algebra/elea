package elea

import java.io.File

import elea.Parser.{DataDef, StatementHandler, TermDef}
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
  val manifesto = s"Elea v$programVersion - a supercompiler for theorem provers"

  val configParser = new OptionParser[Config]("elea") {
    head(manifesto)

    opt[Unit]('s', "stats")
      .action((_, config) => config.copy(recordStats = true))
      .text("print out performance statistics in comments")

    opt[File]('f', "input-file")
        .action((file, config) => config.copy(fromFile = Some(file)))
  }

  val supercompiler = Simplifier.supercompilation

  class CLIStatementHandler(config: Config) extends StatementHandler {
    override def dataDef(dataDef: DataDef): DataDef = {
      println(dataDef.toLisp())
      dataDef
    }

    override def termDef(termDef: TermDef): TermDef = {
      val startTime = System.nanoTime()
      val simplifiedDef = termDef.modifyTerm(supercompiler.run)
      val finishTime = System.nanoTime()

      println(simplifiedDef.toLisp())

      if (config.recordStats) {
        val timeTakenMillis = (finishTime - startTime) / (1000 * 1000)
        println(s";; took ${timeTakenMillis}ms")
      }

      simplifiedDef
    }
  }

  def main(args: Array[String]): Unit = {
    val config = configParser
        .parse(args, Config())
        .getOrElse(throw new IllegalArgumentException("Bad command line parameters"))
    require(config.fromFile.isDefined, "Please provide an input file")
    val input = io.Source.fromFile(config.fromFile.get).mkString
    println(s";; $manifesto")
    val startTime = System.currentTimeMillis()
    run(config, input)
    val timeTakenMillis = System.currentTimeMillis() - startTime
    println(s";; finished (took ${timeTakenMillis}ms)")
  }

  def run(config: Config, inputProgram: String): Unit = {
    implicit val startingProgram = Program.empty
    val handler = new CLIStatementHandler(config)
    Parser.parseAll(inputProgram, handler)
  }
}
