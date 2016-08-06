package hoverboard

import hoverboard.term._

import scalaz.IList

class CriticalPairsTest extends TestConfig {

  "critical pairs" should "be correctly detectable" in {
    val AppView(addFun: Fix, addArgs) = t"Add (Add x y) z".drive
    val addPair = CriticalPair.of(addFun, addArgs)
    addPair.path shouldEqual IList[Case.Index]("add", "add")
    addPair.term shouldEqual t"Add x y".drive

    val AppView(sortFun1: Fix, sortArgs1) = t"IsSorted (Insert n xs)".drive
    val sortPair1 = CriticalPair.of(sortFun1, sortArgs1)
    sortPair1.path shouldEqual IList[Case.Index]("sorted1", "ins1")
    sortPair1.term shouldEqual t"Insert n xs".drive

    val AppView(sortFun2: Fix, sortArgs2) = t"IsSorted (Cons x (Insert n xs))".drive
    val sortPair2 = CriticalPair.of(sortFun2, sortArgs2)
    sortPair2.path shouldEqual IList[Case.Index]("sorted2", "ins1")
    sortPair2.term shouldEqual t"Insert n xs".drive

    val AppView(revFun1: Fix, revArgs1) = t"Reverse (Reverse xs)".drive
    val revPair1 = CriticalPair.of(revFun1, revArgs1)
    revPair1.path shouldEqual IList[Case.Index]("rev", "rev")
    revPair1.term shouldEqual t"Reverse xs".drive

    val AppView(revFun2: Fix, revArgs2) = t"Reverse (Append (Reverse xs) (Cons x Nil))".drive
    val revPair2 = CriticalPair.of(revFun2, revArgs2)
    revPair2.path shouldEqual IList[Case.Index]("rev", "app", "rev")
    revPair2.term shouldEqual t"Reverse xs".drive
  }
}
