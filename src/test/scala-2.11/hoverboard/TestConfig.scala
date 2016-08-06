package hoverboard

import org.scalacheck.Arbitrary
import org.scalatest.prop.PropertyChecks

trait TestConfig extends PropertyChecks {
  // TODO remove this when at 1.0 and I can be bothered to wait for all the test cases to finish!
  override implicit val generatorDrivenConfig = PropertyCheckConfig(minSuccessful = 5)

  implicit val termArb = Arbitrary(Arbitraries.term)
  implicit val program: Program = Program.prelude
}
