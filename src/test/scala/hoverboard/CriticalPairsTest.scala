package hoverboard

import hoverboard.term.CriticalPair.{CaseSplit, Fission, Induction}
import hoverboard.term._

import scalaz.IList

class CriticalPairsTest extends TestConfig {

  import Util._

  def testCriticalPair[T: Manifest](term: Term, expectedPath: IList[Name], expectedExpandedTerm: Term): Unit = {
    val AppView(fix: Fix, args) = term.reduce
    val cp = CriticalPair.of(fix, args)
    cp.action shouldBe a [T]
    cp.path shouldEqual expectedPath.map(Case.Index.fromName)
    cp.action.apply(term).reduce shouldEqual expectedExpandedTerm.reduce
  }

  // Yes, I know this is a table driven test, I just prefer this style
  "critical pairs" should "be correctly detected" in {
    testCriticalPair[Induction](
      term".add (.add x y) z",
      IList("add", "add"),
      term"case x | .0 -> .add y z | .Suc x' -> .add (.add x y) z end")

    testCriticalPair[Induction](
      term".rev (.rev xs)",
      IList("rev", "rev"),
      term"case xs | .Nil -> .Nil | .Cons x xs' -> .rev (.rev xs) end")

    testCriticalPair[Induction](
      term".rev (.app (.rev xs) (.Cons x .Nil))",
      IList("rev", "app", "rev"),
      term"case xs | .Nil -> .rev (.app (.rev xs) (.Cons x .Nil)) | .Cons x' xs' -> .rev (.app (.rev xs) (.Cons x .Nil)) end")

    testCriticalPair[Induction](
      term".isSorted (.insert n xs)",
      IList("sorted1", "ins1"),
      term"case xs | .Nil -> .isSorted (.insert n xs) | .Cons x xs' -> .isSorted (.insert n xs) end")

    testCriticalPair[Induction](
      term".isSorted (.Cons x (.insert n xs))",
      IList("sorted2", "ins1"),
      term"case xs | .Nil -> .isSorted (.Cons x (.Cons n .Nil)) | .Cons x' xs' -> .isSorted (.Cons x (.insert n xs)) end")

    testCriticalPair[Induction](
      term".count n (.app xs ys)",
      IList("count1", "app"),
      term"case xs | .Nil -> .count n ys | .Cons x xs' -> .count n (.app xs ys) end")

    testCriticalPair[CaseSplit](
      term".count n (.Cons x (.app xs ys))",
      IList("count2"),
      term"case .eq n x | .True -> .count n (.Cons x (.app xs ys)) | .False -> .count n (.Cons x (.app xs ys)) end")

    testCriticalPair[Induction](
      term".minus (.minus n m) (.Suc k)",
      IList("minus2", "minus1"),
      term"case m | .0 -> .minus (.minus n m) (.Suc k) | .Suc x' -> .minus (.minus n m) (.Suc k) end")

    testCriticalPair[Induction](
      term".lteq (.count n xs) (.count n (.app xs ys))",
      IList("lteq1", "count1"),
      term"case xs | .Nil -> .True | .Cons x xs' -> .lteq (.count n xs) (.count n (.app xs ys)) end")

    testCriticalPair[CaseSplit](
      term".delete n (.Cons x xs')",
      IList("delete2"),
      term"case .eq n x | .True -> .delete n (.Cons x xs') | .False -> .delete n (.Cons x xs') end")

    testCriticalPair[Fission](
      term".add (.add x (.Suc y)) z",
      IList("add"),
      term".Suc (.add (.add x y) z)")

    testCriticalPair[Fission](
      term".lteq (.add (.Suc x) y) z",
      IList("lteq1"),
      term".lteq (.Suc (.add x y)) z")
  }
}
