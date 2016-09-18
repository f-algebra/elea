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

//  "critiquing" should "work for examples requiring constructor fission" in {
//    import supercompiler.testCritique
//
//    val (revTerm, revSub) = testCritique(ISet.fromList(List(term".rev xs")), term".rev (.snoc n xs)")
//    revSub.boundVars.size shouldBe 1
//    revTerm shouldEqual term".Cons n ${revSub.boundVars.toList.head}"
//  }

  "supercompilation" should "work for simple examples" in {
    supercompile(term".add x y") shouldEqual term".add x y".drive
    supercompile(term".add (.add x y) z") shouldEqual term".add x (.add y z)".drive
    supercompile(term".rev (.app xs (.Cons y .Nil))") shouldEqual term".revSnoc y xs".drive
  }

  Program
    .prelude
    .loadURL(getClass.getResource("test.hover")).definitions
    .filterKeys(_.startsWith("prop"))
    .foreach { case (propName, propTerm) =>
      it should s"prove $propName in test.hover" in {
        val name = propName
        supercompiler.supercompile(propTerm) shouldEqual Truth
      }
    }

//  "critiquing" should "be able to fission out constructor contexts" in {
//    val supercompiler = new Supercompiler with NoSupercompile with NoRipple with TestAssertions
//    supercompiler.assertSuccesfulCritique(ISet.fromList(List(t"Reverse ys")), t"ReverseSnoc ys y")
//  //  rippleWithSuccessCheck(t"Reverse (Reverse xs)", t"Reverse (Append (Reverse ys) (Cons n Nil))")
//    // rippleWithSuccessCheck(t"IsSorted (Flatten t)", t"IsSorted (Append (Flatten t1) (Cons n (Flatten t2)))")
//  }
}
