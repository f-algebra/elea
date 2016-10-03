package hoverboard

import hoverboard.term._
import org.scalatest.prop.TableDrivenPropertyChecks

import scalaz.IList

class CriticalPairsTest extends TestConfig {

  def testCriticalPair(opaqueTerm: Term, expectedPath: CriticalPath): Unit =
    testCriticalPair(opaqueTerm, expectedPath, opaqueTerm)

  def testCriticalPair(term: Term, expectedPath: CriticalPath, expectedTerm: Term): Unit = {
    val AppView(fix: Fix, args) = term.reduce
    val cp = CriticalPair.of(fix, args)
    cp.path shouldEqual expectedPath
    cp.term shouldEqual expectedTerm.reduce
  }

  // Yes, I know this is a table driven test, I just prefer this style
  "critical pairs" should "be correctly detected" in {
    testCriticalPair(
      term".add (.add x y) z",
      CriticalPath.invert(term"x", "add", "add"), term".add x y")

    testCriticalPair(
      term".add (.Suc x) y",
      CriticalPath.invert(term".Suc (.add x y)".reduce))

    testCriticalPair(
      term".add (.Suc (.Suc x)) y",
      CriticalPath.invert(term".Suc (.add (.Suc x) y)".reduce))

    testCriticalPair(
      term".rev (.rev xs)",
      CriticalPath.invert(term"xs", "rev", "rev"), term".rev xs")

    testCriticalPair(
      term".rev (.app (.rev xs) (.Cons x .Nil))",
      CriticalPath.invert(term"xs", "rev", "app", "rev"), term".rev xs")

    testCriticalPair(
      term".isSorted (.insert n xs)",
      CriticalPath.invert(term"xs", "ins1", "sorted1"), term".insert n xs")

    testCriticalPair(
      term".isSorted (.Cons x (.insert n xs))",
      CriticalPath.invert(term"xs", "ins1", "sorted2"), term".insert n xs")

    testCriticalPair(
      term".count n (.app xs ys)",
      CriticalPath.invert(term"xs", "app", "count1"), term".app xs ys")

    testCriticalPair(
      term".count n (.Cons x (.app xs ys))",
      CriticalPath.invert(term".eq n x".reduce, "count2"))
  }
}
