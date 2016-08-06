package hoverboard

import hoverboard.Supercompiler.Env
import hoverboard.term._
import org.scalacheck.Arbitrary
import org.scalatest.prop.PropertyChecks
import org.scalatest.{Matchers, FlatSpec}

class SupercompilerTest extends TestConfig {

  import Util._

  def rippleWithSuccessCheck(skeleton: Term, goal: Term): (Term, Substitution) = {
    val (drivenGoal, drivenSkel) = (goal.drive, skeleton.drive)
    val (term, sub) = Supercompiler.ripple(Env.empty)(drivenSkel, drivenGoal)
    sub should be ('nonEmpty)
    term :/ sub shouldEqual drivenGoal
    sub.toMap.values.foreach { rippled =>
      drivenSkel unifyLeft rippled should be ('isDefined)
    }
    (term, sub)
  }

  "rippling" should "work for known examples" in {
    rippleWithSuccessCheck(t"Add (Add x y) z", t"Add (unfold Add x y) z")
    rippleWithSuccessCheck(t"Reverse (Append xs ys)", t"Reverse (unfold Append xs ys)")
  //  rippleWithSuccessCheck(t"Reverse (Reverse xs)", t"Reverse (Append (Reverse ys) (Cons n Nil))")
    // rippleWithSuccessCheck(t"IsSorted (Flatten t)", t"IsSorted (Append (Flatten t1) (Cons n (Flatten t2)))")
  }
}
