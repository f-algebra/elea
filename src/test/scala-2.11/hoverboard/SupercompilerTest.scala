package hoverboard

import hoverboard.term._
import org.scalacheck.Arbitrary
import org.scalatest.prop.PropertyChecks
import org.scalatest.{Matchers, FlatSpec}

class SupercompilerTest extends FlatSpec with Matchers with PropertyChecks {

  implicit val termArb = Arbitrary(Arbitraries.term)
  implicit val program: Program = Program.prelude

  "unfold" should "correctly unfold strict fixed-points" in {
    Supercompiler.unfold(t"Reverse (Reverse xs)".asInstanceOf[App]) shouldEqual
      t"Reverse (unfold Reverse xs)"
    Supercompiler.unfold(t"Lt (Add x y) (Add n m)".asInstanceOf[App]) shouldEqual
      t"Lt (unfold Add x y) (unfold Add n m)"
  }

  "rippling" should "work for associativity of Add" in {
//    val ripple = Supercompiler.ripple(t"Add (Add x y) z".drive, t"Suc (Add (Add x y) m)".drive)
//    ripple should be ('isDefined)
//    val Some((term, sub)) = ripple
//    sub.boundVars.size shouldBe 1
//    val subVar = Var(sub.boundVars.toList.head)
//    subVar shouldEqual term
//    sub.apply(subVar) shouldEqual t"Add (Add x y) z".drive
  }
}
