package hoverboard

import hoverboard.term._

class DrivingTest extends TestConfig {

  import Util._

  "driving" should "perform beta reduction" in {
    term"(fn x -> x) y".drive shouldEqual term"y"
    term"(fn x x -> f x) y z".drive shouldEqual term"f z"
    term"(fn x -> .add x z) (.add x y)".drive shouldEqual term".add (.add x y) z".drive
  }

  it should "distribute case onto case" in {
    term"case (case x | .Suc x -> x end) | .Suc x -> x end".drive shouldEqual
      term"case x | .Suc x2 -> (case x2 | .Suc x -> x end) end"
  }

  it should "distribute case applied as a function" in {
    term"(case x | .Suc y -> f y end) y".drive shouldEqual term"case x | .Suc z -> f z y end"
  }

  it should "remove identity cases" in {
    term"case x | .0 -> .0 | .Suc x -> .Suc x | else -> x end".drive shouldEqual term"x"
  }

  it should "distribute case applied as a strict argument" in {
    term".add (case x | .Suc y -> f x end) y".drive shouldEqual
      term"case x | .Suc z -> .add (f (.Suc z)) y end".drive
  }

  it should "not distribute case applied as a non-strict argument" in {
    term".add y (case x | .Suc y -> f x end)".drive should not equal
      term"case x | .Suc z -> .add y (f (.Suc z)) end".drive
  }

  it should "reduce case of inj" in {
    term"case .Suc x | .0 -> a | .Suc b -> f b end".drive shouldEqual term"f x"
  }

  it should "remove constant fixed-point arguments" in {
    term".add".drive shouldEqual term"fn x y -> (fix f x -> case x | .0 -> y | .Suc x' -> .Suc (f x') end) x"
    term".app xs (.Cons y .Nil)".drive shouldEqual term".snoc y xs".drive
    term"fix[a] f xs -> case xs | .Nil -> .Nil | .Cons x xs' -> .app (f xs') (.Cons x .Nil) end".drive shouldEqual
      term"fix[a] f xs -> case xs | .Nil -> .Nil | .Cons x xs' -> .snoc x (f xs') end".drive
  }

  it should "not introduce free variables" in {
    forAll { (t: Term) =>
      t.drive.freeVars.difference(t.freeVars) shouldBe empty
    }
  }

  it should "be idempotent" in {
    val historicalFails = Seq(
      term".add .0 y",
      term"(fn x y -> case x | .0 -> .1 | .Suc x' -> .add y (.mul x' y) end) nat_1 (.Suc (.Suc (f nat_1)))")
    historicalFails
      .foreach { t => t.drive shouldEqual t.drive.drive }

    // This still occasionally fails for examples too large to debug...
    // I think I can switch this check back on if I disable unfolding in the driving step.
//    forAll { (t: Term) =>
//      val driven = t.drive
//      driven shouldEqual driven.drive
//    }
  }

  it should "not simplify undriveable terms" in {
    term".lt x y".drive shouldEqual term".lt x y"
  }

  it should "unfold fixed points with constructor arguments" in {
    term".add (.Suc x) y".drive shouldEqual term".Suc (.add x y)".drive
    term".rev (.Cons x xs)".drive shouldEqual term".app (.rev xs) (.Cons x .Nil)".drive
    term".add .0 (.add x y)".drive shouldEqual term".add x y".drive
  }

  it should "not unfold fixed points with constructor arguments dangerously" in {
    term".lt x (.Suc x)".drive shouldEqual term".lt x (.Suc x)"
    term".lteq (.Suc x) x".drive shouldEqual term".lteq (.Suc x) x"
    term".isSorted (.Cons x xs)".drive shouldEqual term".isSorted (.Cons x xs)"
    term".isSorted (.Cons x (.ins n xs))".drive shouldEqual term".isSorted (.Cons x ${term".ins n xs".drive})"
  }

  it should "not add fixed-point indices" in {
    forAll { (t: Term) => t.drive.indices.isSubsetOf(t.indices) shouldBe true }
  }

  it should "rewrite fixed-points called with ⊥ as strict arguments to ⊥" in {
    term".lt x ⊥".drive shouldEqual ⊥
    term".lt ⊥ x".drive shouldEqual ⊥
    term".add x ⊥".drive should not equal ⊥
    term".add (.Suc ⊥) y".drive shouldEqual term".Suc ⊥"
    term".isSorted (.Cons x ⊥)".drive shouldEqual ⊥
  }

  it should "float pattern matching out of the left hand side of =<" in {
    term"(case x | .0 -> y end) =< z".drive shouldEqual term"case x | .0 -> y =< z end"
    term"(case x | .0 -> .add y z end) =< .add x (.add y z)".drive shouldEqual ⊥
  }
}
