package hoverboard

import java.net.URL

import hoverboard.term.Term

import scala.io.Source

/** Collection of term definitions */
class Program(val definitions: Map[String, Term]) {
  def +(newDef: (String, Term)) = {
    require(!definitions.contains(newDef._1), s"Cannot redefine ${newDef._1}")
    Program(definitions + newDef)
  }

  def definitionOf(name: String): Term = {
    require(definitions.contains(name), s"Cannot find definition for $name")
    definitions.get(name).get
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
    Program.empty.loadURL(getClass.getResource("prelude.hover"))
}
