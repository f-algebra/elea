package hoverboard

import org.scalacheck.Arbitrary
import org.scalatest._
import org.scalatest.prop.PropertyChecks

trait TestConfig extends FlatSpec with Matchers with PropertyChecks {
  override implicit val generatorDrivenConfig =
    PropertyCheckConfig(
      // TODO remove this when at 1.0 and I can be bothered to wait for all the test cases to finish!
      minSuccessful = 5,
      workers = 8)

  implicit val termArb = Arbitrary(Arbitraries.term)
  implicit val program: Program = Program.prelude
}
