package hoverboard

import hoverboard.term._
import org.scalacheck.Arbitrary
import org.scalatest.prop.PropertyChecks
import org.scalatest.{FlatSpec, Matchers}

import scalaz.Scalaz._
import scalaz._

class TermTest extends FlatSpec with Matchers with PropertyChecks {

  import Util._
  implicit val program: Program = Program.prelude
  implicit val termArb = Arbitrary(Arbitraries.term)

  "substitution" should "replace variables" in {
    t"x" :/ t"f x" / "x" shouldBe t"f x"
  }

  "constant arguments" should "be detectable" in {
    t"Add".asInstanceOf[Fix].constantArgs shouldEqual IList(1)
    t"Append".asInstanceOf[Fix].constantArgs shouldEqual IList(1)
    t"Reverse".asInstanceOf[Fix].constantArgs shouldEqual IList.empty[Int]
    t"fix x -> x".asInstanceOf[Fix].constantArgs shouldEqual IList.empty[Int]
  }

  they should "be removable" in {
    t"Add".asInstanceOf[Fix].removeConstantArg(1) shouldEqual
      t"fn x y -> (fix f x -> case x | 0 -> y | Suc x' -> Suc (f x') end) x"
    t"Append".asInstanceOf[Fix].removeConstantArg(1) shouldEqual
      t"fn xs ys -> (fix f xs -> case xs | Nil -> ys | Cons x xs' -> Cons x (f xs') end) xs"
    t"fix f x y z -> case y | 0 -> z | Suc y' -> Suc (f x y' z) end".asInstanceOf[Fix].removeConstantArg(0) shouldEqual
      t"fn x -> fix f y z -> case y | 0 -> z | Suc y' -> Suc (f y' z) end"
    t"fix f x y z -> case y | 0 -> z | Suc y' -> Suc (f x y' z) end".asInstanceOf[Fix].removeConstantArg(2) shouldEqual
      t"fn x y z -> (fix f x y -> case y | 0 -> z | Suc y' -> Suc (f x y') end) x y"
  }

  they should "not be detected incorrectly" in {
    t"fix id x -> case x | 0 -> 0 | Suc x -> Suc (id x) end".asInstanceOf[Fix]
      .constantArgs shouldBe empty
    t"Lt".asInstanceOf[Fix]
      .constantArgs shouldBe empty
  }

  "term ordering" should "be reflexive" in {
    forAll { (t: Term) => (t order t) shouldEqual Ordering.EQ }
  }

  "alpha equality" should "be reflexive" in {
    forAll { (t: Term) => t =@= t shouldEqual true }
  }

  "subterms containing" should "correctly descend into terms" in {
    t"Suc (add x' y)".subtermsContaining("add") shouldEqual
      ISet.fromList(List(t"add x' y", t"add"))
  }

  "unifyLeft" should "detect substitution" in {
    forAll { (t1: Term, t2: Term) =>
      t1.freeVars.toList.foreach { x =>
        t1 unifyLeft (t1 :/ (t2 / x)) shouldEqual Some(t2 / x)
      }
    }
  }

  "zipping a term with itself" should "yield its subterms" in {
    forAll { (t: Term) =>
      val zipped = t zip t
      zipped.isDefined shouldEqual true
      zipped.get.reverse shouldEqual t.immediateSubterms.zip(t.immediateSubterms)
    }
  }

  "lambdas" should "remove free variables" in {
    forAll { (t: Term) =>
      t.freeVars.toList.foreach { x =>
        Lam(x, t).freeVars shouldEqual t.freeVars.delete(x)
      }
    }
  }

  "subterm mapping" should "hit every subterm" in {
    forAll { (t: Term) =>
      var hit = ISet.empty[Term]
      t.mapSubterms { t => hit = hit.insert(t); t }
      hit shouldEqual ISet.fromFoldable(t.subterms)
    }
  }

  "explore" should "correctly list potential return values of (Add x y)" in {
    val exploredAdd = t"Add x y".exploreSet
    exploredAdd should contain (t"y")
    exploredAdd should contain (t"Suc y")
    exploredAdd should not contain t"0"
  }

  it should "correctly list potential return values of (Reverse (Snoc y xs))" in {
    val unfused = t"Reverse (Snoc y xs)".exploreSet
    unfused should contain (t"_|_")
    unfused should contain (t"Cons y Nil")
    unfused should not contain t"Nil"
  }

  it should "correctly list potential return values for Reverse fused into Snoc" in {
    val fused = t"(fix f xs -> case xs | Nil -> Cons y Nil | Cons x xs' -> Append (f xs') (Cons x Nil) end) xs".exploreSet
    fused should contain (t"_|_")
    fused should contain (t"Cons y Nil")
    fused should not contain t"Nil"
  }

  "mapBranchesWithBindings" should "descend into case-of branches" in {
    var seen = ISet.empty[(ISet[Name], Term)]
    t"case x | 0 -> a | Suc x' -> case x' | 0 -> b | Suc x'' -> c end end"
      .mapBranchesWithBindings { (bindings, term) =>
        seen = seen.insert((bindings, term))
        term
      }
    seen shouldEqual ISet.fromList[(ISet[Name], Term)](List(
      (ISet.empty[Name], Var("a")),
      (ISet.singleton(Name("x'")), Var("b")),
      (ISet.fromList(List(Name("x'"), Name("x''"))), Var("c"))))
  }

  "stripContext" should "strip (Cons y _) from an example" in {
    t"case xs | Nil -> Cons y Nil | Cons x xs' -> Cons y (Append (f xs') (Cons x Nil)) end"
      .stripContext(t"Cons y _") shouldEqual Some(t"case xs | Nil -> Nil | Cons x xs' -> Append (f xs') (Cons x Nil) end")
  }

  it should "fail for invalid examples" in {
    t"case xs | Nil -> Cons y Nil | Cons x xs' -> Cons z (Append (f xs') (Cons x Nil)) end"
      .stripContext(t"Cons y _") shouldBe empty
    t"case xs | Nil -> Cons y Nil | Cons y xs' -> Cons y (Append (f xs') (Cons x Nil)) end"
      .stripContext(t"Cons y _") shouldBe empty
  }

  "guessConstructorContext" should "find the context for Reverse fused into Snoc" in {
    t"fix f xs -> case xs | Nil -> Cons y Nil | Cons x xs' -> Append (f xs') (Cons x Nil) end"
      .asInstanceOf[Fix].guessConstructorContext should contain (t"Cons y _")
  }

  "fissionConstructorContext" should "fission (Cons y _) out of Reverse fused into Snoc" in {
    t"fix[a] f xs -> case xs | Nil -> Cons y Nil | Cons x xs' -> Append (f xs') (Cons x Nil) end"
      .drive
      .asInstanceOf[Fix]
      .fissionConstructorContext shouldEqual Some(
        t"fix[a] f xs -> case xs | Nil -> Nil | Cons x xs' -> Append (f xs') (Cons x Nil) end".drive
      )
  }

  "homeomorphic embedding" should "work properly" in {
    t"x" couplingRule t"Lt x (Suc x)" shouldBe false
    t"Lt x (Suc x)" strictlyEmbedsInto t"Lt x (Suc x)" shouldBe false
    t"Lt x (Suc x)" couplingRule t"Lt x (Suc x)" shouldBe true
  }

  "fppf" should "be recognisable" in {
    t"Add x y".asInstanceOf[App].isFPPF shouldBe true
    t"Add x x".drive.asInstanceOf[App].isFPPF shouldBe false
    t"Add (Mul x y) z".drive.asInstanceOf[App].isFPPF shouldBe false
    t"Add x (Mul y z)".drive.asInstanceOf[App].isFPPF shouldBe true
  }

  "strict args" should "be recognisable" in {
    t"Reverse".asInstanceOf[Fix].strictArgs shouldEqual IList(0)
    t"Append".asInstanceOf[Fix].strictArgs shouldEqual IList(0)
    t"Lt".asInstanceOf[Fix].strictArgs shouldEqual IList(0, 1)
  }

  def msgWithSanityCheck(t1: Term, t2: Term): (Term, Substitution, Substitution) = {
    val (ctx, sub1, sub2) = t1 ⨅ t2
    ctx :/ sub1 shouldEqual t1
    ctx :/ sub2 shouldEqual t2
    sub1.toMap.keySet shouldEqual sub2.toMap.keySet
    (ctx, sub1, sub2)
  }

  "most specific generalisation" should "expose substitutions" in {
    forAll { (ctx: Term, leftSubTerm: Term, rightSubTerm: Term) =>
      whenever(!leftSubTerm.isInstanceOf[Var]) {
        whenever(!rightSubTerm.isInstanceOf[Var]) {
          // A lot of properties turn out not to hold if the two substitution terms are zippable
          whenever(leftSubTerm.zip(rightSubTerm).isEmpty) {
            ctx.freeVars.toList.foreach { subVar =>
              val leftTerm = ctx :/ leftSubTerm / subVar
              val rightTerm = ctx :/ rightSubTerm / subVar
              val (msgCtx, leftSub, rightSub) = msgWithSanityCheck(leftTerm, rightTerm)
              ISet.fromFoldable(leftSub.toMap.values) shouldEqual ISet.singleton(leftSubTerm)
              ISet.fromFoldable(rightSub.toMap.values) shouldEqual ISet.singleton(rightSubTerm)
              val uniOpt = msgCtx.unifyLeft(ctx)
              uniOpt should be('isDefined)
              val Some(ctxUni) = uniOpt
              ISet.fromFoldable(ctxUni.toMap.values) shouldEqual ISet.singleton(Var(subVar))
              leftSub.toMap.keySet shouldEqual ctxUni.boundVars
              rightSub.toMap.keySet shouldEqual ctxUni.boundVars
            }
          }
        }
      }
    }
  }

  it should "do nothing for equal terms" in {
    forAll { (t: Term) =>
      val (ctx, sub1, sub2) = msgWithSanityCheck(t, t)
      sub1.isEmpty shouldBe true
      sub2.isEmpty shouldBe true
      ctx shouldEqual t
    }
  }
}
