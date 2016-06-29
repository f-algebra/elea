package hoverboard

import hoverboard.term.{Var, Term}
import org.scalacheck.{Arbitrary, Gen}

import scalaz.ISet

object Arbitraries {
  private implicit val prelude = Program.prelude

  private val allTerms = Seq(
    t"_|_",
    t"fix x -> x",
    t"f nat_1",
    t"p bool_1"
  )

  private val natTerms = Seq(
    t"0",
    t"Suc nat_1",
    t"Add nat_1 nat_2",
    t"Mul nat_1 nat_2",
    t"unfold Add nat_1 nat_2",
    t"unfold Mul nat_1 nat_2",
    t"case nat_1 | 0 -> nat_2 | Suc y -> nat_3 end",
    t"f nat_1",
    t"case bool_1 | True -> nat_1 | False -> nat_2 end"
  )

  private val boolTerms = Seq(
    t"False",
    t"True",
    t"Lt nat_1 nat_2",
    t"case nat_1 | 0 -> bool_1 | Suc y -> bool_2 end"
  )

  private def mutateTerm(term: Term): Gen[Term] = for {
    substName <- Gen.oneOf(term.freeVars.toList)
    substTerm <- substName.name.take(4) match {
      case "nat_" => natTerm
      case "bool_" => boolTerm
      case _ => Gen.const(Var(substName))
    }
  } yield term :/ substTerm / substName

  implicit class GenTermWrapper(gen: Gen[Term]) {
    def withMutation: Gen[Term] = for {
      term <- gen
      mutated <- Gen.choose(0f, 1f).flatMap(x => if (x < 0.3f) mutateTerm(term) else Gen.const(term))
    } yield mutated
  }

  def natTerm: Gen[Term] = Gen.oneOf(allTerms ++ natTerms).withMutation
  def boolTerm: Gen[Term] = Gen.oneOf(allTerms ++ boolTerms).withMutation
  def term: Gen[Term] = Gen.oneOf(natTerm, boolTerm)
}
