package hoverboard

import hoverboard.term._
import org.scalatest.prop.TableDrivenPropertyChecks

import scalaz.IList

class CriticalPairsTest extends TestConfig {

  import Util._

  def testCriticalPair(term: Term, expectedPath: IList[Name], expectedMatchedTerm: Term): Unit = {
    val AppView(fix: Fix, args) = term.reduce
    val cp = CriticalPair.of(fix, args)
    cp.path shouldEqual expectedPath.map(Case.Index.fromName)
    cp.action.caseOf.matchedTerm shouldEqual expectedMatchedTerm.reduce
  }

  // Yes, I know this is a table driven test, I just prefer this style
  "critical pairs" should "be correctly detected" in {
    testCriticalPair(
      term".add (.add x y) z",
      IList("add", "add"), term"x")

    testCriticalPair(
      term".rev (.rev xs)",
      IList("rev", "rev"), term"xs")

    testCriticalPair(
      term".rev (.app (.rev xs) (.Cons x .Nil))",
      IList("rev", "app", "rev"), term"xs")

    testCriticalPair(
      term".isSorted (.insert n xs)",
      IList("sorted1", "ins1"), term"xs")

    testCriticalPair(
      term".isSorted (.Cons x (.insert n xs))",
      IList("sorted2", "ins1"), term"xs")

    testCriticalPair(
      term".count n (.app xs ys)",
      IList("count1", "app"), term"xs")

    testCriticalPair(
      term".count n (.Cons x (.app xs ys))",
      IList("count2"), term".eq n x")

    testCriticalPair(
      term".minus (.minus n m) (.Suc k)",
      IList("minus2", "minus1"), term"m")

    testCriticalPair(
      term".lteq (.count n xs) (.count n (.app xs ys))",
      IList("lteq1", "count1"), term"xs")
  }
}
