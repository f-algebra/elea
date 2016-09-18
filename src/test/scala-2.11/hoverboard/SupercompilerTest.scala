package hoverboard

import hoverboard.term._

class SupercompilerTest extends TestConfig {

  import Util._

  val supercompiler = new Supercompiler
  import supercompiler._

  def testRipple(skeleton: Term, goal: Term, context: Term): Unit = {
    val (rippledSkeletons, rippledGoal, rippleSub) = ripple(skeleton.drive, goal.drive)
    rippledSkeletons.toList.foreach(_ shouldBe a [Var])
    context.apply(rippledSkeletons).drive shouldEqual rippledGoal
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
  }

  it should "work for examples requiring constructor fission" in {
    testRipple(
      term".rev (.rev xs)",
      term".rev (.snoc n (.rev xs2))",
      term"fn ys -> .Cons n ys")
  }

  // All properties in test_properties.hover should pass
  Program
    .prelude
    .loadURL(getClass.getResource("test_properties.hover")).definitions
    .filterKeys(_.startsWith("prop"))
    .foreach { case (propName, propTerm) =>
      it should s"prove $propName in test_properties.hover" in {
        supercompiler.supercompile(propTerm) shouldEqual Truth
      }
    }

  // All properties in false_test_properties.hover should fail
  Program
    .prelude
    .loadURL(getClass.getResource("false_test_properties.hover")).definitions
    .filterKeys(_.startsWith("prop"))
    .foreach { case (propName, propTerm) =>
      it should s"fail to prove $propName in false_test_properties.hover" in {
        supercompiler.supercompile(propTerm) should not equal Truth
      }
    }
}
