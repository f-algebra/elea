package hoverboard

import hoverboard.rewrite.{Env, Prover, Simplifier}
import hoverboard.term.Pattern
import org.scalatest.{FlatSpec, Matchers}
import org.scalatest.prop.PropertyChecks

class ProverTest extends FlatSpec with Matchers with PropertyChecks with TestConfig {
  val reductionProver = new Prover(Simplifier.reduction)

  "prover backed by reduction" should "prove simple unsatifiability examples" in {
    val unfoldLteqEnv = Env.empty
      .withMatch(term"x", Pattern.from(term".Suc x'"))
      .withMatch(term"y", Pattern.from(term".0"))
      .withMatch(term".lteq x y", Pattern.from(term".True"))
    reductionProver.unsatisfiable(unfoldLteqEnv) shouldBe true

    val unfoldLteqEnvRev = Env.empty
      .withMatch(term".lteq x y", Pattern.from(term".True"))
      .withMatch(term"x", Pattern.from(term".Suc x'"))
      .withMatch(term"y", Pattern.from(term".0"))
    reductionProver.unsatisfiable(unfoldLteqEnvRev) shouldBe true
  }
}
