package hoverboard

import hoverboard.Supercompiler.Env
import hoverboard.term._

import scalaz.IList

class SupercompilerTest extends TestConfig {

  import Util._

  val ripplingOnly = new Supercompiler {
    override def supercompile(env: Env, term: Term): Term = term
  }

  val supercompiler = new Supercompiler
  import supercompiler._

  def testRipple(skeleton: Term, goal: Term, context: Term): Unit = {
    val (rippledSkeletons, rippledGoal, rippleSub) =
      // This is to test rippling only! Don't want to involve supercompilation of sub-ripples
      ripplingOnly.ripple(skeleton.reduce, goal.reduce)
    rippledSkeletons.toList.foreach(_ shouldBe a [Var])
    context.apply(rippledSkeletons).reduce shouldEqual rippledGoal
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

  it should "fail for prop_zeno3 shaped example" in {
    val skel = term".lteq (.count n xs) (.count n (.app xs ys))".reduce
    val goal = term".lteq (.count n xs') (.count n (.app (.Cons x xs') ys))".reduce
    val (rippledSkels, rippledGoal, sub) = ripplingOnly.ripple(skel, goal)
    rippledSkels shouldBe empty
    rippledGoal should not equal goal
    (rippledGoal unifyLeft goal) should not be empty
  }

  "supercompilation" should "unfold fixed-points with constructor arguments" in {
    supercompile(term".app (.Cons a (.Cons b (.Cons c xs))) ys") shouldEqual
      term".Cons a (.Cons b (.Cons c (.app xs ys)))".reduce
  }

  // All properties in test_properties.hover should pass
  Program
    .prelude
    .loadURL(getClass.getResource("test_properties.hover")).definitions
    .filterKeys(_.startsWith("prop"))
  //  .filterKeys(_ == "prop_zeno1")
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
