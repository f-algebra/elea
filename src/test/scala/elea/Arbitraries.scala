package elea

import elea.term.{Var, Term}
import org.scalacheck.{Arbitrary, Gen}

import scalaz.ISet

object Arbitraries {
  private implicit val prelude = Program.prelude

  private val allTerms = Seq(
    term"_|_",
    term"fix x -> x",
    term"f nat_1",
    term"p bool_1"
  )

  private val natTerms = Seq(
    term".0",
    term".Suc nat_1",
    term".add nat_1 nat_2",
    term".mul nat_1 nat_2",
    term"unfold .add nat_1 nat_2",
    term"unfold .mul nat_1 nat_2",
    term"case nat_1 | .0 -> nat_2 | .Suc y -> nat_3 end",
    term"f nat_1",
    term"case bool_1 | .True -> nat_1 | .False -> nat_2 end",
    term".count nat_1 list_1"
  )

  private val listTerms = Seq(
    term".app list_1 list_2",
    term".rev list_1",
    term".Nil",
    term".Cons nat_1 list_1"
  )

  private val boolTerms = Seq(
    term".False",
    term".True",
    term".lt nat_1 nat_2",
    term"case nat_1 | .0 -> bool_1 | .Suc y -> bool_2 end"
  )

  private def prefixToGen(name: String): Gen[Term] = {
    if (name.startsWith("nat_"))
      natTerm
    else if (name.startsWith("bool_"))
      boolTerm
    else if (name.startsWith("list_"))
      listTerm
    else
      Gen.const(Var(name))
  }

  private def mutateTerm(term: Term): Gen[Term] = for {
    substName <- Gen.oneOf(term.freeVars.toList)
    substTerm <- prefixToGen(substName.name)
  } yield term :/ substTerm / substName

  implicit class GenTermWrapper(gen: Gen[Term]) {
    def withMutation: Gen[Term] = for {
      term <- gen
      size <- Gen.size
      mutated <-
        if (size > 0 && term.freeVars.size > 0)
          Gen.resize(size - 1, mutateTerm(term))
        else
          Gen.const(term)
    } yield mutated
  }

  def natTerm: Gen[Term] = Gen.oneOf(allTerms ++ natTerms).withMutation
  def boolTerm: Gen[Term] = Gen.oneOf(allTerms ++ boolTerms).withMutation
  def listTerm: Gen[Term] = Gen.oneOf(allTerms ++ listTerms).withMutation
  def term: Gen[Term] = Gen.oneOf(natTerm, boolTerm)
}
