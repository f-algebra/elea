package hoverboard

import hoverboard.term._

class SupercompilerTest extends TestConfig {

  import Util._

  val supercompiler = new Supercompiler
  import supercompiler._

  def testRipple(skeleton: Term, goal: Term, context: Term): Unit = {
    val (rippledSkeletons, rippledGoal, rippleSub) = ripple(skeleton.reduce, goal.reduce)
    rippledSkeletons.toList.foreach(_ shouldBe a [Var])
    context.apply(rippledSkeletons).reduce shouldEqual rippledGoal
  }
/*
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
*/
  "supercompilation" should "unfold fixed-points with constructor arguments" in {
    supercompile(term".app (.Cons a (.Cons b (.Cons c xs))) ys") shouldEqual
      term".Cons a (.Cons b (.Cons c (.app xs ys)))".reduce
  }

  // All properties in test_properties.hover should pass
  Program
    .prelude
    .loadURL(getClass.getResource("test_properties.hover")).definitions
    .filterKeys(_.startsWith("prop"))
    .toSeq.sortBy(_._1)
    .foreach { case (propName, propTerm) =>
      it should s"prove $propName in test_properties.hover" in {
        val propNameVar = propName
        supercompile(propTerm) shouldEqual Logic.Truth
      }
    }

  // All properties in unprovable_test_properties.hover should fail
  Program
    .prelude
    .loadURL(getClass.getResource("unprovable_test_properties.hover")).definitions
    .filterKeys(_.startsWith("prop"))
    .toSeq.sortBy(_._1)
    .foreach { case (propName, propTerm) =>
      it should s"fail to prove $propName in unprovable_test_properties.hover" in {
        supercompile(propTerm) should not equal Logic.Truth
      }
    }
}
