package hoverboard

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
}

object Program {
  def apply(definitions: Map[String, Term]) =
    new Program(definitions)

  val empty = Program(Map.empty)

  lazy val prelude: Program = {
    val preludeText = Source.fromURL(getClass.getResource("prelude.hover")).mkString
    Parser.parseAll(preludeText)(_.modifyTerm(_.drive))(Program.empty)
  }
}
