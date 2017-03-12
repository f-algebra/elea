package elea

import elea.term._

import scalaz.Scalaz.intInstance
import scalaz.{IList, ISet, NonEmptyList, Scalaz}

/**
  * Parses our lisp-style untyped lambda calculus with fixed-points and algebraic data-types
  */
object Parser {
  sealed trait Statement extends LispRepresentable {
    def apply(program: Program): Program
  }

  trait StatementHandler {
    def termDef(termDef: TermDef): TermDef
    def dataDef(dataDef: DataDef): DataDef
  }

  object ReducingHandler extends StatementHandler {
    override def termDef(termDef: TermDef): TermDef = termDef.modifyTerm(_.reduce)
    override def dataDef(dataDef: DataDef): DataDef = dataDef
  }

  case class TermDef(name: String, term: Term) extends Statement {
    def apply(program: Program) = program + (name -> term.withName(name))

    def modifyTerm(f: Term => Term): TermDef =
      copy(term = f(term))

    override def toLisp(settings: LispPrintSettings): String = {
      val (vars, body) = term.flattenLam
      s"(defun $name ${vars.toList.map(_.toString).mkString(" ")} ${body.toLisp(settings)})"
    }
  }

  case class DataDef(name: String, constructors: Seq[Constructor]) extends Statement {
    def apply(program: Program) =
      program ++ constructors.map(c => c.name.toString -> c)

    override def toLisp(settings: LispPrintSettings): String =
      s"(defdata $name${constructors.map(_.toDefinitionLisp(name)).mkString(" ", " ", "")})"
  }


  private class Rules(program: Program){
    import fastparse.WhitespaceApi
    import fastparse.noApi._

    val whitespace = {
      import fastparse.all._
      lazy val commentBody: P[Unit] = P(CharsWhile(_ != '*') ~/ "*" ~/ ("/" | commentBody))
      (CharIn(" \n\r") | "/*" ~ commentBody).rep
    }

    val White = WhitespaceApi.Wrapper(whitespace)
    import White._

    val keywords = Set("fix", "fun", "match", "else", "data", "let", "end", "rec", "unfold", "assert", "false", "true", "in")

    val lowercase = P(CharIn('a' to 'z') | CharIn(Seq('_', 'α')))
    val uppercase = P(CharIn('A' to 'Z') | CharIn('0' to '9') | CharIn(Seq('\'')))
    val int: P[Int] = P((P(CharIn('1' to '9')) ~~ P(CharIn('0' to '9')).repX).!).map(Integer.parseInt(_))
    val identifier: P[String] = P((uppercase | lowercase).repX(1).!).filter(name => !keywords.contains(name))
    val freshener: P[Int] = P("[" ~ int ~ "]")
    val name: P[Name] = P(identifier ~ freshener.?)
        .map { case (name, freshener) => Name(name, freshener) }

    val fixIndex: P[Name] = P("[" ~ name ~ "]")
    val caseIndex: P[Case.Index] = P(("[" ~ name ~ "]").?).map(_.map(Case.Index.fromName).getOrElse(Case.Index.fresh))

    val termVar: P[Term] = P(name)
        .map { name =>
          program.definitionOf(name.name) match {
            case Some(defn) if name.freshener.isEmpty => defn
            case _ => Var(name)
          }
        }

    val bot: P[Term] = P(("_|_" | "⊥") ~/).map(_ => Bot)
    val truth: P[Term] = P("true" ~/).map(_ => Logic.Truth)
    val falsity: P[Term] = P("false" ~/).map(_ => Logic.Falsity)

    val unfold: P[Term] = P("unfold" ~/ term).map(_.unfold)
    val fix: P[Fix] = P("fix" ~/ fixIndex.? ~ name.rep(1) ~/ term)
      .map { case (idx, vars, body) =>
        Fix(Lam(IList(vars: _*).toNel.get, body), idx.map(Fix.finite).getOrElse(Fix.freshOmegaIndex))
      }
    val lam: P[Term] = P("fun" ~/ name.rep(1) ~ term)
      .map { case (vars, body) => Lam(IList(vars : _*), body) }
    val app: P[Term] = P(term ~/ term.rep)
      .map { case (func, args) => func.apply(IList(args: _*)) }

    val lteq: P[Term] = P("=<" ~/ term ~/ term).map { case (left, right) => Leq(left, right) }
    val gteq: P[Term] = P(">=" ~/ term ~/ term).map { case (left, right) => Leq(right, left) }
    val and: P[Term] = P("&&" ~/ term ~/ term).map { case (left, right) => Logic.and(left, right) }
    val eq: P[Term] = P("==" ~/ term ~/ term).map { case (left, right) => Logic.equality(left, right) }
    val or: P[Term] = P("||" ~/ term ~/ term).map { case (left, right) => Logic.or(left, right) }
    val negation: P[Term] = P("!" ~/ term).map(Logic.not)
    val prop: P[Term] = negation | lteq | gteq | and | eq | or

    val caseOf: P[Case] = P("match" ~/ caseIndex ~ term ~ branch.rep(1)).map(m => Case(m._2, IList(m._3 : _*).toNel.get, m._1))
    val nakedTerm: P[Term] = P(prop | fix | lam | app | caseOf | termVar | term)
    val term: P[Term] = P(bot | truth | falsity | termVar | ("(" ~/ nakedTerm ~ ")" ~/))

    val pattern: P[Pattern] = P(termVar ~ name.rep)
      .map(m => Pattern(m._1.asInstanceOf[Constructor], IList(m._2 : _*)))

    val branch: P[Branch] = P("(" ~/ pattern ~/ "->" ~/ nakedTerm ~ ")" ~/).map(m => PatternBranch(m._1, m._2))

    def constructorDef: P[Constructor] = {
      val typeArg: P[Boolean] = P("?").map(_ => false) | P(name).map(_ => true)  // TODO properly check that this is the name of the type we are defining
      P("(" ~ identifier ~ typeArg.rep ~ ")").map {
        case (name, tyArgs) =>
          val recArgs = tyArgs.indices.filter(i => tyArgs(i))
          Constructor(name, tyArgs.length, ISet.fromList(List(recArgs : _*)))
      }
    }

    val defdata: P[Statement] = P("(" ~ "defdata" ~/ identifier ~/ constructorDef.rep ~ ")" ~/)
      .map { case (name, cons) => DataDef(name, cons) }

    val defun: P[Statement] = P("(" ~ "defun" ~/ identifier ~/ name.rep ~ term ~ ")")
        .map { case (name, vars, body) =>
          TermDef(name, Fix(Lam(NonEmptyList(Name(name), vars: _*), body), Fix.freshOmegaIndex))
        }

    val statement: P[Option[Statement]] =
      P(whitespace ~ (P(defdata | defun).map(Some(_)) | P(End).map(_ => None)))
  }

  /**
    * Parses the first definition in the given string.
    */
  def parseStatement(text: String)(implicit program: Program): Option[(Statement, String)] = {
    val parsed = new Rules(program).statement.parse(text).get
    parsed.value match {
      case None => None
      case Some(stmt) => Some((stmt, text.substring(parsed.index)))
    }
  }

  /**
    * Parses statements one by one from a string.
    * @return the program with all statements loaded
    */
  def parseAll(text: String, handler: StatementHandler = ReducingHandler)(implicit program: Program): Program =
    Scalaz.unfold((text, program)) { case (text, program) =>
      parseStatement(text)(program).map { case (stmt, remaining) =>
        val newProgram = stmt match {
          case stmt: TermDef => handler.termDef(stmt)(program)
          case stmt: DataDef => handler.dataDef(stmt)(program)
        }
        (newProgram, (remaining, newProgram))
      }
    }.last

  def parseTerm(text: String)(implicit program: Program): Term =
    new Rules(program).term.parse(text).get.value
}
