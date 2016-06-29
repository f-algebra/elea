package hoverboard

import hoverboard.term._
import org.scalacheck.Arbitrary
import org.scalatest.prop.PropertyChecks
import org.scalatest.{Matchers, FlatSpec}

class SupercompilerTest extends FlatSpec with Matchers with PropertyChecks {

  implicit val termArb = Arbitrary(Arbitraries.term)
  implicit val program: Program = Program.prelude

  "unfold" should "correctly unfold strict fixed-points" in {
    Supercompiler.unfold(t"Reverse (Reverse xs)") shouldEqual
      t"Reverse (unfold Reverse xs)"
    Supercompiler.unfold(t"Lt (Add x y) (Add n m)") shouldEqual
      t"Lt (unfold Add x y) (unfold Add n m)"
  }

  it should "unfold fixed-points without arguments" in {
    Supercompiler.unfold(t"Ones") shouldEqual t"unfold Ones"
    Supercompiler.unfold(t"Reverse Ones") shouldEqual t"Reverse (unfold Ones)"
  }

  def rippleWithSuccessCheck(skeleton: Term, goal: Term): (Term, Substitution) = {
    val (drivenGoal, drivenSkel) = (goal.drive, skeleton.drive)
    val (term, sub) = Supercompiler.ripple(drivenSkel, drivenGoal)
    term :/ sub shouldEqual drivenGoal
    sub.toMap.values.foreach { rippled =>
      drivenSkel unifyLeft rippled should be ('isDefined)
    }
    (term, sub)
  }

  "rippling" should "work for known examples" in {
   // rippleWithSuccessCheck(t"Add (Add x y) z", t"Suc (Add (Add x2 y) z)")
    rippleWithSuccessCheck(t"Reverse (Reverse xs)", t"Reverse (Append ys (Cons n Nil))")
  }
}
