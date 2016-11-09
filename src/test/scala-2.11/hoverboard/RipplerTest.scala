package hoverboard

import hoverboard.rewrite._
import hoverboard.term.{Term, Var}

class RipplerTest extends TestConfig {

  import Util._

  val rippler = new Rippler(new Critiquer(Simplifier.reducer))

  def testRipple(skeleton: Term, goal: Term, context: Term): Unit = {
    val ripple = rippler.run(skeleton.reduce, goal.reduce)
    ripple.skeletons.toList.foreach(_ shouldBe a [Var])
    context.apply(ripple.skeletons).reduce shouldEqual ripple.goal
  }

  "rippling" should "work for simple examples" in {
    testRipple(
      term".add (.add x y) z",
      term".Suc (.add (.add x2 y) z)",
      term".Suc")

    testRipple(
      term".rev (.app xs ys)",
      term".app (.rev (.app xs2 ys)) (.Cons x .Nil)",
      term"fn xs -> .app xs (.Cons x .Nil)")

    testRipple(
      term".minus (.minus n m) (.Suc y)",
      term".minus (.minus x' y') (.Suc y)",
      term"fn x -> x")
  }

  it should "fail for prop_zeno3 shaped example" in {
    val skel = term".lteq (.count n xs) (.count n (.app xs ys))".reduce
    val goal = term".lteq (.count n xs') (.count n (.app (.Cons x xs') ys))".reduce
    val ripple = rippler.run(skel, goal)
    ripple.skeletons shouldBe empty
    ripple.goal should not equal goal
    (ripple.goal unifyLeft goal) should not be empty
  }
}
