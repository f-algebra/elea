package hoverboard

import hoverboard.term._
import scalaz._
import Scalaz._

sealed trait Statement {
  def apply(program: Program): Program
}

case class TermDef(name: String, term: Term) extends Statement {
  def apply(program: Program) = program + (name -> term.withName(name))

  def modifyTerm(f: Term => Term): TermDef =
    copy(term = f(term))
}

case class ConstructorDef(constr: Constructor) extends Statement {
  def apply(program: Program) =
    program + (constr.name.toString -> constr)
}

object OldParser {

  private sealed trait BinOp

  private object BinOp {

    case object Leq extends BinOp
    case object Geq extends BinOp
    case object Eq extends BinOp
    case object And extends BinOp
    case object Or extends BinOp
  }

  private class Rules(program: Program) {
    import fastparse.WhitespaceApi
    import fastparse.noApi._

    val whitespace = {
      import fastparse.all._
      lazy val commentBody: P[Unit] = P(CharsWhile(_ != '*') ~/ "*" ~/ ("/" | commentBody))
      (CharIn(" \n\r") | "/*" ~ commentBody).rep
    }

    val White = WhitespaceApi.Wrapper(whitespace)
    import White._

    val keywords = Set("fix", "fn", "case", "of", "else", "data", "let", "end", "rec", "unfold", "assert", "false", "true", "in")

    val lowercase = P(CharIn('a' to 'z') | CharIn(Seq('_', 'α')))
    val uppercase = P(CharIn('A' to 'Z') | CharIn('0' to '9') | CharIn(Seq('\'')))
    val int: P[Int] = P((P(CharIn('1' to '9')) ~~ P(CharIn('0' to '9')).repX).!).map(Integer.parseInt(_))
    val freshener: P[Int] = P("[" ~ int ~ "]")

    val varName: P[Name] = P(P(lowercase ~~ (uppercase | lowercase).repX).! ~ freshener.?)
      .filter(n => !keywords.contains(n._1) || n._2.isDefined)
      .map(n => Name(n._1, n._2))
    val definitionName: P[String] = P((uppercase | lowercase).repX(1).!)

    val definedTerm: P[Term] = P("." ~~ definitionName ~/).map(n => program.definitionOf(n).get)
    val fixIndex: P[Name] = P("[" ~ varName ~ "]")
    val caseIndex: P[Case.Index] = P(("[" ~ varName ~ "]").?).map(_.map(Case.Index.fromName).getOrElse(Case.Index.fresh))

    val termVar: P[Term] = P(varName).map(Var)
    val simpleTerm: P[Term] = P(truth | falsity | bot | unfold | termVar | definedTerm | "(" ~ term ~ ")")
    val unfold: P[Term] = P("unfold" ~/ definedTerm).map(_.unfold)
    val fix: P[Fix] = P("fix" ~/ fixIndex.? ~ varName.rep(1) ~ "->" ~/ term)
      .map(m => Fix(Lam(IList(m._2 : _*).toNel.get, m._3), m._1.map(Fix.finite).getOrElse(Fix.freshOmegaIndex)))
    val lam: P[Term] = P("fn" ~/ varName.rep(1) ~ "->" ~/ term).map(m => Lam(IList(m._1 : _*), m._2))
    val app: P[Term] = P(simpleTerm ~ simpleTerm.rep).map(m => m._1(m._2 : _*))
    val bot: P[Term] = P(("_|_" | "⊥") ~/).map(_ => Bot)
    val truth: P[Term] = P("true" ~/).map(_ => Logic.Truth)
    val falsity: P[Term] = P("false" ~/).map(_ => Logic.Falsity)
    val negation: P[Term] = P("!" ~/ term).map(Logic.not)
    val binOp: P[BinOp] =
      P("=<").map(_ => BinOp.Leq) |
        P(">=").map(_ => BinOp.Geq) |
        P("==").map(_ => BinOp.Eq) |
        P("&&").map(_ => BinOp.And) |
        P("||").map(_ => BinOp.Or)
    val prop: P[Term] = P(app ~ binOp ~/ term).map { m =>
      val left = m._1
      val right = m._3
      m._2 match {
        case BinOp.Leq => Leq(left, right)
        case BinOp.Geq => Leq(right, left)
        case BinOp.Eq => Logic.equality(left, right)
        case BinOp.And => Logic.and(left, right)
        case BinOp.Or => Logic.or(left, right)
      }
    }
    val assertion: P[Case] = P("assert" ~/ pattern ~ "<-" ~/ term ~ "in" ~/ term).map {
      case (pattern, matchedTerm, branchTerm) =>
        val branches = NonEmptyList[Branch](
          PatternBranch(pattern, branchTerm),
          DefaultBranch(Logic.Truth))
        Case(matchedTerm, branches, Case.Index.fresh)
    }
    val caseOf: P[Case] = P("case" ~/ caseIndex ~ term ~ branch.rep(1) ~ "end" ~/).map(m => Case(m._2, IList(m._3 : _*).toNel.get, m._1))
    val term: P[Term] = P(NoCut(prop) | negation | assertion | fix | lam | app | caseOf)

    val pattern: P[Pattern] = P(definedTerm ~ varName.rep).map(m => Pattern(m._1.asInstanceOf[Constructor], IList(m._2 : _*)))

    val branch: P[Branch] = {
      val defaultBranch: P[Branch] = P("else" ~/ "->" ~/ term).map(DefaultBranch)
      val patternBranch: P[Branch] = P(pattern ~/ "->" ~/ term).map(m => PatternBranch(m._1, m._2))
      P("|" ~/ (defaultBranch | patternBranch))
    }

    val constructorDef: P[Constructor] = {
      val typeArg: P[Boolean] = P("*").map(_ => true) | P("_").map(_ => false)
      P(definitionName ~ typeArg.rep).map { m =>
        val recArgs = m._2.indices.filter(i => m._2(i))
        Constructor(Name(m._1), m._2.length, ISet.fromList(List(recArgs : _*)))
      }
    }

    val data: P[Statement] = P("data" ~/ constructorDef).map(ConstructorDef)

    val letrec: P[Statement] = for {
      (defName, vars, body) <- P("let" ~ "rec" ~/ definitionName ~ varName.rep ~ "=" ~/ term)
    } yield TermDef(defName, Fix(Lam(NonEmptyList(Name(defName), vars: _*), body), Fix.freshOmegaIndex))

    val let: P[Statement] = for {
      (defName, vars, body) <- P("let" ~ definitionName ~ varName.rep ~ "=" ~/ term)
    } yield TermDef(defName, Lam(IList(vars: _*), body))

    val statement: P[Option[Statement]] =
      P(whitespace ~ (P(data | letrec | let).map(Some(_)) | P(End).map(_ => None)))
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
    * Parses statements one by one from a string
    * @return Successive [[Program]] objects after each statement has been loaded.
    */
  def parseAll(text: String)(termHandler: TermDef => TermDef)(implicit program: Program): Program =
    Scalaz.unfold((text, program)) { case (text, program) =>
      parseStatement(text)(program).map { case (stmt, remaining) =>
        val newProgram = stmt match {
          case stmt: TermDef => termHandler(stmt)(program)
          case _ => stmt(program)
        }
        (newProgram, (remaining, newProgram))
      }
    }.last

  def parseTerm(text: String)(implicit program: Program): Term =
    new Rules(program).term.parse(text).get.value
}
