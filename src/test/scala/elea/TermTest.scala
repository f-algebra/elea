package elea

import elea.term._
import org.scalacheck._

import scalaz.{Name => _, _}
import Scalaz._

class TermTest extends TestConfig {

  import Util._

  "substitution" should "replace variables" in {
    term"x" :/ term"f x" / "x" shouldBe term"f x"
  }

  it should "correctly merge" in {
    (term"x" / "y") ++ (term"y" / "x") shouldEqual Some(term"x" / "y")
  }

  "constant arguments" should "be detectable" in {
    term"fix f x y z -> case y | .0 -> z | .Suc y' -> .Suc (f x y' z) end"
      .asInstanceOf[Fix].constantArgs shouldEqual IList(0, 2)
    term"fix x -> x"
      .asInstanceOf[Fix].constantArgs shouldEqual IList.empty[Int]
  }

  they should "be removed by reduction" in {
    term".add" shouldEqual
      term"fn x y -> (fix f x -> case x | .0 -> y | .Suc x' -> .Suc (f x') end) x"
    term".app" shouldEqual
      term"fn xs ys -> (fix f xs -> case xs | .Nil -> ys | .Cons x xs' -> .Cons x (f xs') end) xs"
    term"fix f x y z -> case y | .0 -> z | .Suc y' -> .Suc (f x y' z) end".asInstanceOf[Fix].removeConstantArg(0) shouldEqual
      term"fn x -> fix f y z -> case y | .0 -> z | .Suc y' -> .Suc (f y' z) end"
    term"fix f x y z -> case y | .0 -> z | .Suc y' -> .Suc (f x y' z) end".asInstanceOf[Fix].removeConstantArg(2) shouldEqual
      term"fn x y z -> (fix f x y -> case y | .0 -> z | .Suc y' -> .Suc (f x y') end) x y"
  }

  they should "not be detected incorrectly" in {
    term"fix id x -> case x | .0 -> .0 | .Suc x -> .Suc (id x) end".asInstanceOf[Fix]
      .constantArgs shouldBe empty
    term".lt".asInstanceOf[Fix]
      .constantArgs shouldBe empty
  }

  "term ordering" should "be reflexive" in {
    forAll { (t: Term) => (t order t) shouldEqual Ordering.EQ }
  }

  "alpha equality" should "be reflexive" in {
    forAll { (t: Term) => t =@= t shouldEqual true }
  }

  it should "respect bindings" in {
    term"fn y -> x" =@= term"fn x -> x" shouldBe false
  }

  "term equality" should "correctly fail for this stupid historical bug" in {
    term".Suc y" == term"f y" shouldBe false
  }

  it should "respect variable bindings" in {
    term"fn y -> x" == term"fn x -> x" shouldBe false
  }

  it should "imply alpha equality" in {
    forAll { (t1: Term, t2: Term) =>
      whenever (!(t1 =@= t2)) {
        t1 == t2 shouldBe false
      }
    }
  }

  "subterms containing" should "correctly descend into terms" in {
    term".Suc (add x' y)".subtermsContaining("add") shouldEqual
      ISet.fromList(List(term"add x' y", term"add"))
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
      zipped.get shouldEqual t.immediateSubterms.zip(t.immediateSubterms)
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

  "immediate subterms" should "be preserved under identity" in {
    forAll { (t: Term) =>
      t.mapImmediateSubterms(t => t) shouldEqual t
      t.withImmediateSubterms(t.immediateSubterms) shouldEqual t
    }
  }

  "freshening" should "preserve alpha-equality" in {
    forAll { (t: Term) =>
      t.freshen shouldEqual t
    }
  }

  "explore" should "correctly list potential return values of (.add x y)" in {
    val exploredAdd = term".add x y".exploreSet
    exploredAdd should contain (term"y")
    exploredAdd should contain (term".Suc y")
    exploredAdd should not contain term".0"
  }

  it should "correctly list potential return values of (Reverse (Snoc y xs))" in {
    val unfused = term".rev (.snoc y xs)".exploreSet
    unfused should contain (term"_|_")
    unfused should contain (term".Cons y .Nil")
    unfused should not contain term".Nil"
  }

  it should "correctly list potential return values for Reverse fused into Snoc" in {
    val fused = term"(fix f xs -> case xs | .Nil -> .Cons y .Nil | .Cons x xs' -> .app (f xs') (.Cons x .Nil) end) xs".exploreSet
    fused should contain (term"_|_")
    fused should contain (term".Cons y .Nil")
    fused should not contain term".Nil"
  }

  it should "correctly list potential values for omega" in {
    val omega = term"fix omega -> .Suc omega".reduce
    omega.exploreSet should contain (term".Suc (.Suc _|_)")
  }

  "mapBranchesWithBindings" should "descend into case-of branches" in {
    var seen = ISet.empty[(ISet[Name], Term)]
    term"case x | .0 -> a | .Suc x' -> case x' | .0 -> b | .Suc x'' -> c end end"
      .mapBranchesWithBindings { (bindings, term) =>
        seen = seen.insert((bindings, term))
        term
      }
    seen shouldEqual ISet.fromList[(ISet[Name], Term)](List(
      (ISet.empty[Name], Var("a")),
      (ISet.singleton(Name("x'")), Var("b")),
      (ISet.fromList(List(Name("x'"), Name("x''"))), Var("c"))))
  }

  "stripContext" should "strip (.Cons y _) correctly" in {
    val ctx = C(x => term".Cons y ${Var(x)}")

    ctx.strip(term".Cons y .Nil") shouldEqual Some(term".Nil")

    ctx.strip(term"case xs | .Nil -> .Cons y .Nil | .Cons x xs' -> .Cons y (.app (f xs') (.Cons x .Nil)) end") shouldEqual
      Some(term"case xs | .Nil -> .Nil | .Cons x xs' -> .app (f xs') (.Cons x .Nil) end")

    ctx.strip(term"case xs | .Nil -> _|_ | .Cons x xs' -> .Cons y (.app (f xs') (.Cons x .Nil)) end") shouldEqual
      Some(term"case xs | .Nil -> _|_ | .Cons x xs' -> .app (f xs') (.Cons x .Nil) end")
  }

  it should "fail for invalid examples" in {
    C(x => term".Cons y"(Var(x)))
      .strip(term"case xs | .Nil -> .Cons y .Nil | .Cons x xs' -> .Cons z (.app (f xs') (.Cons x .Nil)) end") shouldBe empty
    C(x => term".Cons y"(Var(x)))
      .strip(term"case xs | .Nil -> .Cons y .Nil | .Cons y xs' -> .Cons y (.app (f xs') (.Cons x .Nil)) end") shouldBe empty
  }

  it should "return _|_ when stripping constants" in {
    val ctx = C(_ => term".True")
    ctx.strip(term".True").get shouldEqual term"_|_"
    ctx.strip(term"_|_").get shouldEqual term"_|_"
    ctx.strip(term"case x | .0 -> .True | .Suc x' -> _|_ end").get shouldEqual
      term"case x | .0 -> _|_ | .Suc x' -> _|_ end"

    ctx.strip(term".False") shouldEqual None
    ctx.strip(term"case x | .0 -> .True | .Suc x' -> .False end") shouldEqual None
  }

  "guessConstructorContext" should "find the context for Reverse fused into Snoc" in {
    term"fix f xs -> case xs | .Nil -> .Cons y .Nil | .Cons x xs' -> .app (f xs') (.Cons x .Nil) end"
      .asInstanceOf[Fix].guessConstructorContext should contain (C(x => term".Cons y"(Var(x))))
  }

  "fissionConstructorContext" should "fission (.Cons y _) out of .reverse fused into .snoc" in {
    val Some((ctx, newFix)) = term".revSnoc y"
      .reduce
      .asInstanceOf[Fix]
      .fissionConstructorContext
    ctx shouldEqual C(x => term".Cons y ${Var(x)}")
    val otherFix = term"fix rev xs -> case xs | .Nil -> .Nil | .Cons x xs' -> .snoc x (rev xs') end".reduce
    otherFix shouldEqual (newFix: Term)
  }

  it should "reduce constant functions to their constants" in {
    val constantTrue = term"fix f n -> case n | .0 -> .True | .Suc x' -> f x' end".reduce.asInstanceOf[Fix]
    constantTrue.guessConstructorContext.get shouldEqual C(_ => term".True")
    val Some((ctx, newFix)) = constantTrue.fissionConstructorContext
    ctx shouldEqual C(_ => term".True")
    (newFix: Term).reduce shouldEqual Bot
  }

  it should "not reduce functions without simplifying them (actually this is okay for now since fission is used with the homeo-embedding check)" in {
    val omega = term"fix omega -> .Suc omega end".reduce.asInstanceOf[Fix]
    omega.guessConstructorContext.get shouldEqual C(x => term".Suc ${Var(x)}")
    // omega.fissionConstructorContext shouldBe empty
  }

  "homeomorphic embedding" should "work properly" in {
    term"x" couplingRule term".lt x (.Suc x)" shouldBe false
    term".lt x (.Suc x)" strictlyEmbedsInto term".lt x (.Suc x)" shouldBe false
    term".lt x (.Suc x)" couplingRule term".lt x (.Suc x)" shouldBe true
  }

  "fppf" should "be recognisable" in {
    term".add x y".reduce.asInstanceOf[App].isFPPF shouldBe true
    term".add x x".reduce.asInstanceOf[App].isFPPF shouldBe false
    term".add (.mul x y) z".reduce.asInstanceOf[App].isFPPF shouldBe false
    term".add x (.mul y z)".reduce.asInstanceOf[App].isFPPF shouldBe true
    term".minus n m".reduce.asInstanceOf[App].isFPPF shouldBe true
  }

  "strict args" should "be recognisable" in {
    term".add x y".reduce.leftmost.asInstanceOf[Fix].strictArgIndices shouldEqual IList(0)
    term".rev".asInstanceOf[Fix].strictArgIndices shouldEqual IList(0)
    term".lt".asInstanceOf[Fix].strictArgIndices shouldEqual IList(0, 1)
  }

  "replace" should "reverse substitution" in {
    forAll { (t1: Term, t2: Term) =>
      whenever(!t1.subtermSet.contains(t2)) {
        t1.freeVars.toList.foreach { (x: Name) =>
          (t1 :/ t2 / x).replace(t2, Var(x)) shouldEqual t1
        }
      }
    }
  }

  "generalisation" should "work for a simple example" in {
    val term = term"case x | .Suc y -> f (g y) (.Suc z) a end"
    val (genTerm, genVars) = term.generalise(IList(term"g y", term".Suc z", term"a"))
    genVars.length shouldBe 3
    val Seq(x1, x2, x3) = genVars.toList
    val uni = genTerm unifyLeft term
    uni should be ('isDefined)
    val uniMap = uni.get.toMap
    uniMap.keySet shouldEqual ISet.fromList(List(x2, x3))
    genTerm :/ Substitution(x1 -> term"g y", x2 -> term".Suc z", x3 -> term"a") shouldEqual term
  }

  "AppPrefix" should "work" in {
    val AppPrefix(t1, t2, xs) = (term"f x y z", term"g a")
    t1 shouldEqual term"f x"
    t2 shouldEqual term"g a"
    xs shouldEqual NonEmptyList(term"y", term"z")
  }

  "freshenIndices" should "not leave any indices behind" in {
    forAll { (t: Term) =>
      t.freshenIndices.caseIndexSet.intersection(t.caseIndexSet) shouldEqual
        ISet.empty[Case.Index]
    }
  }

  it should "not affect equality" in {
    forAll { (t: Term) =>
      t.freshenIndices shouldEqual t
    }
  }

  "argument subterms" should "contain all arguments" in {
    // Looks like a weird test but this property broke historically
    forAll { (ts: Seq[Term]) =>
      val tss = ISet.unions(ts.toList.map(t => t.freeSubtermSet.insert(t)))
      ts.foreach(t => tss should contain (t))
    }
  }

  "isProductive" should "work" in {
    term".takeWhile p".reduce.asInstanceOf[Fix].isProductive shouldBe true
    term".dropWhile p".reduce.asInstanceOf[Fix].isProductive shouldBe false
    term".count n".reduce.asInstanceOf[Fix].isProductive shouldBe false
    term".filter p".reduce.asInstanceOf[Fix].isProductive shouldBe false
    term".map f".reduce.asInstanceOf[Fix].isProductive shouldBe true
    term"fix x -> .Suc x".asInstanceOf[Fix].isProductive shouldBe true
  }
}
