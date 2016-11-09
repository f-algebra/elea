package hoverboard

import hoverboard.rewrite._
import hoverboard.term._

import scalaz.IList

class SupercompilerTest extends TestConfig {

  import Util._

  val scc = Simplifier.supercompiler

  "supercompilation" should "unfold fixed-points with constructor arguments" in {
    scc.run(term".app (.Cons a (.Cons b (.Cons c xs))) ys") shouldEqual
      term".Cons a (.Cons b (.Cons c (.app xs ys)))".reduce
  }

  it should "simplify" in {
    val t = scc.run(term".lteq n (.add n m)")
    true shouldBe true
  }

  // All properties in test_properties.hover should pass
  Program
    .prelude
    .loadURL(getClass.getResource("test_properties.hover")).definitions
    .filterKeys(_.startsWith("prop"))
    .toSeq.sortBy(_._1)
    .foreach { case (propName, propTerm) =>
      it should s"prove $propName in test_properties.hover" in {
        val propNameVar = propName
        scc.run(propTerm) shouldEqual Logic.Truth
      }
    }

  // All properties in unprovable_test_properties.hover should fail
  Program
    .prelude
    .loadURL(getClass.getResource("unprovable_test_properties.hover")).definitions
    .filterKeys(_.startsWith("prop"))
    .toSeq.sortBy(_._1)
    .foreach { case (propName, propTerm) =>
      it should s"fail to prove $propName in unprovable_test_properties.hover" in {
        scc.run(propTerm) should not equal Logic.Truth
      }
    }
}
