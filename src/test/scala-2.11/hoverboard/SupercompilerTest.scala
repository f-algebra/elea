package hoverboard

import hoverboard.Supercompiler.{Fold, Env}
import hoverboard.term._
import org.scalacheck.Arbitrary
import org.scalatest.prop.PropertyChecks
import org.scalatest.{Matchers, FlatSpec}

import scalaz.{IList, ISet}

class SupercompilerTest extends TestConfig {

  import Util._

//  trait NoSupercompile extends Supercompiler {
//    override def supercompile(env: Env, term: Term): Term = term
//  }
//
//  trait NoCritique extends Supercompiler {
//    override def critique(env: Env)(skeletons: ISet[Term], goal: Term): (Term, Substitution) =
//      (goal, Substitution.empty)
//  }
//
//  trait NoRipple extends Supercompiler {
//    override def ripple(env: Env)(skeleton: Term, goal: Term): (Term, Substitution) =
//      (goal, Substitution.empty)
//  }

  class TestSupercompiler extends Supercompiler {
    def testRipple(skeleton: Term, goal: Term): (Fold, Term) = {
      val (drivenGoal, drivenSkel) = (goal.drive, skeleton.drive)
      val AppView(skelFix: Fix, skelArgs) = drivenSkel
      val fakeCp = CriticalPair(IList.empty, skelFix, skelArgs)
      val fold = Fold.fromCriticalPair(fakeCp)
      val (term, sub) = ripple(Env.empty, fold)(drivenSkel, drivenGoal)
      (fold, term :/ sub)
    }

    def assertSuccesfulCritique(skeletons: ISet[Term], goal: Term): Unit = {
      val (drivenGoal, drivenSkels) = (goal.drive, skeletons.map(_.drive))
      val (term, sub) = critique(Env.empty)(drivenSkels, drivenGoal)
      sub should be ('nonEmpty)
      term :/ sub shouldEqual drivenGoal
      ISet.fromList(sub.toMap.values) shouldEqual skeletons
    }
  }

  "rippling" should "work for simple examples" in {
    val supercompiler = new TestSupercompiler

    val (addFold, addTerm) = supercompiler.testRipple(t"Add (Add x y) z", t"Suc (Add (Add x2 y) z)")
    addTerm shouldEqual t"Suc (${Var(addFold.foldVar)} x2 y z)"
   // supercompiler.assertSuccessfulRipple(t"Reverse (Append xs ys)", t"Append (Reverse (Append xs2 ys)) (Cons x Nil)")
  }

//  "critiquing" should "be able to fission out constructor contexts" in {
//    val supercompiler = new Supercompiler with NoSupercompile with NoRipple with TestAssertions
//    supercompiler.assertSuccesfulCritique(ISet.fromList(List(t"Reverse ys")), t"ReverseSnoc ys y")
//  //  rippleWithSuccessCheck(t"Reverse (Reverse xs)", t"Reverse (Append (Reverse ys) (Cons n Nil))")
//    // rippleWithSuccessCheck(t"IsSorted (Flatten t)", t"IsSorted (Append (Flatten t1) (Cons n (Flatten t2)))")
//  }
}
