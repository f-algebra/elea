package elea

import elea.term._
import java.net.URL
import scala.io.Source

/** Collection of term definitions */
class Program(val definitions: Map[String, Term]) {
  def +(newDef: (String, Term)) = {
    require(!definitions.contains(newDef._1), s"Cannot redefine ${newDef._1}")
    Program(definitions + newDef)
  }

  def ++(newDefs: Seq[(String, Term)]): Program =
    newDefs.foldLeft(this)(_ + _)

  def definitionOf(name: String): Option[Term] =
    definitions.get(name)

  def loadURLOld(url: URL): Program = {
    val text = Source.fromURL(url).mkString
    OldParser.parseAll(text)(_.modifyTerm(_.reduce))(this)
  }

  def loadURL(url: URL): Program = {
    val text = Source.fromURL(url).mkString
    Parser.parseAll(text)(_.modifyTerm(_.reduce))(this)
  }
}

object Program {
  def apply(definitions: Map[String, Term]) =
    new Program(definitions)

  val empty = Program(Map.empty)

  lazy val prelude: Program =
    Program.empty.loadURLOld(getClass.getResource("prelude.elea"))

  lazy val newPrelude: Program =
    Program.empty.loadURL(getClass.getResource("new-prelude.elea"))
}
