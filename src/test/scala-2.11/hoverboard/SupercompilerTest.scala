package hoverboard

import hoverboard.Supercompiler.{Fold, Env}
import hoverboard.term._
import org.scalacheck.Arbitrary
import org.scalatest.prop.PropertyChecks
import org.scalatest.{Matchers, FlatSpec}

import scalaz.{IList, ISet}

class SupercompilerTest extends TestConfig {

  import Util._

  val supercompiler = new Supercompiler
  import supercompiler._

  def testRipple(skeleton: Term, goal: Term, context: Context, gap: Term): Unit = {
    val (rippleTerm, rippleSub) = ripple(skeleton.drive, goal.drive)
    rippleSub.size shouldEqual 1
    val Seq((rippleVar, rippleGap)) = rippleSub.toMap.toList
    rippleTerm shouldEqual context.apply(Var(rippleVar)).drive
    rippleGap shouldEqual gap.drive
  }

  "rippling" should "work for simple examples" in {
    testRipple(
      term".add (.add x y) z",
      term".Suc (.add (.add x2 y) z)",
      C(x => term".Suc ${Var(x)}"),
      term".add (.add x2 y) z")

    testRipple(
      term".rev (.app xs ys)",
      term".app (.rev (.app xs2 ys)) (.Cons x .Nil)",
      C(x => term".app ${Var(x)} (.Cons x .Nil)"),
      term".rev (.app xs2 ys)")
  }

  it should "work for examples requiring constructor fission" in {
    testRipple(
      term".rev (.rev xs)",
      term".rev (.snoc n (.rev xs2))",
      C(x => term".Cons n ${Var(x)}"),
      term".rev (.rev xs2)")
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
