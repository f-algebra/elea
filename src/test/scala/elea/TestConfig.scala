package elea

import org.scalacheck.Arbitrary
import org.scalatest._
import org.scalatest.prop.PropertyChecks

trait TestConfig extends FlatSpec with Matchers with PropertyChecks {

  override implicit val generatorDrivenConfig =

    PropertyCheckConfig(
      // TODO remove this low minSuccessful value when at v1.0 and I can be bothered to wait for all the test cases to finish!
      minSuccessful = 64,
      workers = 8,
      maxDiscarded = 2000)

  implicit val termArb = Arbitrary(Arbitraries.term)
  implicit val program: Program = Program.prelude
}
