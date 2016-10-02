package hoverboard

import hoverboard.term._
import org.scalatest.prop.TableDrivenPropertyChecks

import scalaz.IList

class CriticalPairsTest extends TestConfig with TableDrivenPropertyChecks {

  val tests = Table[Term, IList[Case.Index], Term](
    ("term", "critical path", "critical term"),
    (term".add (.add x y) z", IList("add", "add"), term".add x y"),
    (term".isSorted (.insert n xs)", IList("sorted1", "ins1"), term".insert n xs"),
    (term".isSorted (.Cons x (.insert n xs))", IList("sorted2", "ins1"), term".insert n xs"),
    (term".rev (.rev xs)", IList("rev", "rev"), term".rev xs"),
    (term".rev (.app (.rev xs) (.Cons x .Nil))", IList("rev", "app", "rev"), term".rev xs")
  )

  "critical pairs" should "be correctly detectable" in {
    forAll (tests) { (term, criticalPath, criticalTerm) =>
      val AppView(fix: Fix, args) = term.reduce
      val cp = CriticalPair.of(fix, args)
      cp.path shouldEqual criticalPath
      cp.term shouldEqual criticalTerm.reduce
    }
  }
}
