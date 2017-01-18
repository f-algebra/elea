package elea

import elea.term._
import org.scalacheck.Arbitrary
import org.scalatest._
import org.scalatest.prop.PropertyChecks

class ParserTest extends FlatSpec with Matchers with PropertyChecks with TestConfig {

  "the old term parser" should "work" in {
    term"(fn x x -> f x) y z" shouldBe Lam("x", Lam("x", Var("f")(Var("x"))))(Var("y"), Var("z"))
  }

  "the new term parser" should "parse new-prelude.elea" in {
    val prelude = Program.newPrelude
    prelude.definitionOf("add") should not be empty
  }

  it should "have toString as a left identity" in {
    // TODO but it doesn't because of variable fresheners not parsing properly, fix me please
//    forAll { (t: Term) =>
//      Parser.parseTerm(t.toString) shouldEqual t
//    }
  }
}
