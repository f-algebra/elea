package hoverboard

import hoverboard.term._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{Await, Future}
import scala.concurrent.duration._

class ReductionTest extends TestConfig {

  import Util._

  "reduction" should "perform beta reduction" in {
    term"(fn x -> x) y".reduce shouldEqual term"y"
    term"(fn x x -> f x) y z".reduce shouldEqual term"f z"
    term"(fn x -> .add x z) (.add x y)".reduce shouldEqual term".add (.add x y) z".reduce
  }

  it should "distribute case onto case" in {
    term"case (case x | .Suc x -> x end) | .Suc x -> x end".reduce shouldEqual
      term"case x | .Suc x2 -> (case x2 | .Suc x -> x end) end"
  }

  it should "distribute case applied as a function" in {
    term"(case x | .Suc y -> f y end) y".reduce shouldEqual term"case x | .Suc z -> f z y end"
  }

  it should "remove identity cases" in {
    term"case x | .0 -> .0 | .Suc x -> .Suc x | else -> x end".reduce shouldEqual term"x"
  }

  it should "distribute case applied as a strict argument" in {
    term".add (case x | .Suc y -> f x end) y".reduce shouldEqual
      term"case x | .Suc z -> .add (f (.Suc z)) y end".reduce
  }

  it should "not distribute case applied as a non-strict argument" in {
    term".add y (case x | .Suc y -> f x end)".reduce should not equal
      term"case x | .Suc z -> .add y (f (.Suc z)) end".reduce
  }

  it should "reduce case of inj" in {
    term"case .Suc x | .0 -> a | .Suc b -> f b end".reduce shouldEqual term"f x"
  }

  it should "remove constant fixed-point arguments" in {
    term".add".reduce shouldEqual term"fn x y -> (fix f x -> case x | .0 -> y | .Suc x' -> .Suc (f x') end) x"
    term".app xs (.Cons y .Nil)".reduce shouldEqual term".snoc y xs".reduce
    term"fix[a] f xs -> case xs | .Nil -> .Nil | .Cons x xs' -> .app (f xs') (.Cons x .Nil) end".reduce shouldEqual
      term"fix[a] f xs -> case xs | .Nil -> .Nil | .Cons x xs' -> .snoc x (f xs') end".reduce
  }

  it should "not introduce free variables" in {
    forAll { (t: Term) =>
      t.reduce.freeVars.difference(t.freeVars) shouldBe empty
    }
  }

  it should "be idempotent" in {
    val historicalFails = Seq(
      term".add .0 y",
      term"(fn x y -> case x | .0 -> .1 | .Suc x' -> .add y (.mul x' y) end) nat_1 (.Suc (.Suc (f nat_1)))")
    historicalFails
      .foreach { t => t.reduce shouldEqual t.reduce.reduce }

    forAll { (t: Term) =>
      val reduced = t.reduce
      reduced shouldEqual reduced.reduce
    }
  }

  it should "not simplify unreduceable terms" in {
    term".lt x y".reduce shouldEqual term".lt x y"
  }

  it should "unfold fixed points with constructor arguments" in {
    term".add (.Suc x) y".reduce shouldEqual term".Suc (.add x y)".reduce
    term".rev (.Cons x xs)".reduce shouldEqual term".app (.rev xs) (.Cons x .Nil)".reduce
    term".add .0 (.add x y)".reduce shouldEqual term".add x y".reduce

    term".count n (.Cons x xs)".reduce shouldEqual
      term"case .eq n x | .True -> .Suc (.count n xs) | .False -> .count n xs end".reduce
  }

  it should "not unfold fixed points with constructor arguments dangerously" in {
    term".lt x (.Suc x)".reduce shouldEqual term".lt x (.Suc x)"
    term".lteq (.Suc x) x".reduce shouldEqual term".lteq (.Suc x) x"
    term".isSorted (.Cons x xs)".reduce shouldEqual term".isSorted (.Cons x xs)"
    term".isSorted (.Cons x (.insert n xs))".reduce shouldEqual term".isSorted (.Cons x ${term".insert n xs".reduce})"
  }

  it should "not add fixed-point indices" in {
    forAll { (t: Term) => t.reduce.indices.isSubsetOf(t.indices) shouldBe true }
  }

  it should "rewrite fixed-points called with ⊥ as strict arguments" in {
    term".lt x ⊥".reduce shouldEqual ⊥
    term".lt ⊥ x".reduce shouldEqual ⊥
    term".add x ⊥".reduce should not equal ⊥
    term".add ⊥ y".reduce shouldEqual ⊥
  }

  it should "float pattern matching out of the left hand side of =<" in {
    term"(case x | .0 -> y end) =< z".reduce shouldEqual term"case x | .0 -> y =< z end"
    term"(case x | .0 -> .add y z end) =< .add x (.add y z)".reduce shouldEqual ⊥
  }

  it should "not beta-reduce forever" in {
    val test = Future {
      term"(fn x -> x x) (fn x -> x x)".reduce shouldEqual term"(fn x -> x x) (fn x -> x x)"
    }
    Await.result(test, 100 milliseconds)
  }

  it should "apply pattern matches as rewrites" in {
    term"case .eq x y | .True -> case .eq x y | .True -> x | .False -> y end end".reduce shouldEqual
      term"case .eq x y | .True -> x end"

    term"case .rev xs | .Cons y ys -> case ys | .Cons z zs -> .rev xs end end".reduce shouldEqual
      term"case .rev xs | .Cons y ys -> case ys | .Cons z zs -> .Cons y (.Cons z zs) end end"
  }

  it should "perform logical reduction" in {
    term"true || p".reduce shouldEqual term"true"
    term"p || true".reduce shouldEqual term"true"
    term"false || p".reduce shouldEqual term"p"
    term"p || false".reduce shouldEqual term"p"

    term"false && p".reduce shouldEqual term"false"
    term"p && false".reduce shouldEqual term"false"
    term"p && true".reduce shouldEqual term"p"
    term"true && p".reduce shouldEqual term"p"

    term"not (not p)".reduce shouldEqual term"p"
  }

  it should "simplify pattern matches which always return _|_" in {
    term"case x | .0 -> _|_ | .Suc x' -> _|_ end".reduce shouldEqual term"_|_"
    term"assert .True <- .eq n m in assert .False <- .eq n m in false".reduce shouldEqual term"_|_"
  }

  it should "reduce _|_ returning fixed-points to _|_" in {
    term"fix f n -> case n | .0 -> _|_ | .Suc x -> _|_ end".reduce shouldEqual term"_|_"
  }

  it should "remove pointless fixes" in {
    term"fix f x -> case x | .0 -> .True | .Suc x' -> .False end".reduce shouldEqual
      term"fn x -> case x | .0 -> .True | .Suc x' -> .False end"
  }
}
