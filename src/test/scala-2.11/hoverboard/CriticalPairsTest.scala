package hoverboard

import hoverboard.term._
import org.scalatest.prop.TableDrivenPropertyChecks

import scalaz.IList

class CriticalPairsTest extends TestConfig {

  def testCriticalPair(term: Term, expectedPath: CriticalPath, expectedMatchedTerm: Term): Unit = {
    val AppView(fix: Fix, args) = term.reduce
    val cp = CriticalPair.of(fix, args)
    cp.isDefined shouldBe true
    cp.get.path shouldEqual expectedPath
    cp.get.caseOf.matchedTerm shouldEqual expectedMatchedTerm.reduce
  }

  // Yes, I know this is a table driven test, I just prefer this style
  "critical pairs" should "be correctly detected" in {
    testCriticalPair(
      term".add (.add x y) z",
      CriticalPath.reverse(term"x", "add", "add"), term"x")

    testCriticalPair(
      term".rev (.rev xs)",
      CriticalPath.reverse(term"xs", "rev", "rev"), term"xs")

    testCriticalPair(
      term".rev (.app (.rev xs) (.Cons x .Nil))",
      CriticalPath.reverse(term"xs", "rev", "app", "rev"), term"xs")

    testCriticalPair(
      term".isSorted (.insert n xs)",
      CriticalPath.reverse(term"xs", "ins1", "sorted1"), term"xs")

    testCriticalPair(
      term".isSorted (.Cons x (.insert n xs))",
      CriticalPath.reverse(term"xs", "ins1", "sorted2"), term"xs")

    testCriticalPair(
      term".count n (.app xs ys)",
      CriticalPath.reverse(term"xs", "app", "count1"), term"xs")

    testCriticalPair(
      term".count n (.Cons x (.app xs ys))",
      CriticalPath.reverse(term".eq n x".reduce, "count2"), term".eq n x")

    testCriticalPair(
      term".minus (.minus n m) (.Suc k)",
      CriticalPath.reverse(term"m", "minus1", "minus2"), term"m")

    testCriticalPair(
      term".lteq (.count n xs) (.count n (.app xs ys))",
      CriticalPath.reverse(term"xs", "count1", "lteq1"), term"xs")
  }
}
