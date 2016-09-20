package hoverboard

import hoverboard.term._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{Await, Future}
import scala.concurrent.duration._

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

    forAll { (t: Term) =>
      val driven = t.drive
      driven shouldEqual driven.drive
    }
  }

  it should "not simplify undriveable terms" in {
    term".lt x y".drive shouldEqual term".lt x y"
  }

  it should "unfold fixed points with constructor arguments" in {
    term".add (.Suc x) y".drive shouldEqual term".Suc (.add x y)".drive
    term".rev (.Cons x xs)".drive shouldEqual term".app (.rev xs) (.Cons x .Nil)".drive
    term".add .0 (.add x y)".drive shouldEqual term".add x y".drive

    term".count n (.Cons x xs)".drive shouldEqual
      term"case .eq n x | .True -> .Suc (.count n xs) | .False -> .count n xs end"
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

  it should "rewrite fixed-points called with ⊥ as strict arguments" in {
    term".lt x ⊥".drive shouldEqual ⊥
    term".lt ⊥ x".drive shouldEqual ⊥
    term".add x ⊥".drive should not equal ⊥
    term".add ⊥ y".drive shouldEqual ⊥
  }

  it should "float pattern matching out of the left hand side of =<" in {
    term"(case x | .0 -> y end) =< z".drive shouldEqual term"case x | .0 -> y =< z end"
    term"(case x | .0 -> .add y z end) =< .add x (.add y z)".drive shouldEqual ⊥
  }

  it should "not beta-reduce forever" in {
    val test = Future {
      term"(fn x -> x x) (fn x -> x x)".drive shouldEqual term"(fn x -> x x) (fn x -> x x)"
    }
    Await.result(test, 100 milliseconds)
  }

  it should "apply pattern matches as rewrites" in {
    term"case .eq x y | .True -> case .eq x y | .True -> x | .False -> y end end".drive shouldEqual
      term"case .eq x y | .True -> x end"

    term"case .rev xs | .Cons y ys -> case ys | .Cons z zs -> .rev xs end end".drive shouldEqual
      term"case .rev xs | .Cons y ys -> case ys | .Cons z zs -> .Cons y (.Cons z zs) end end"
  }

  it should "perform logical reduction" in {
    term"true || p".drive shouldEqual term"true"
    term"p || true".drive shouldEqual term"true"
    term"false || p".drive shouldEqual term"p"
    term"p || false".drive shouldEqual term"p"

    term"false && p".drive shouldEqual term"false"
    term"p && false".drive shouldEqual term"false"
    term"p && true".drive shouldEqual term"p"
    term"true && p".drive shouldEqual term"p"

    term"not (not p)".drive shouldEqual term"p"
  }
}
