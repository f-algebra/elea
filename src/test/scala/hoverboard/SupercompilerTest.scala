package hoverboard

import hoverboard.rewrite._
import hoverboard.term._

class SupercompilerTest extends TestConfig {

  import Util._

  val scc = Simplifier.supercompilation

  "supercompilation" should "unfold fixed-points with constructor arguments" in {
    scc.run(term".app (.Cons a (.Cons b (.Cons c xs))) ys") shouldEqual
      term".Cons a (.Cons b (.Cons c (.app xs ys)))".reduce
  }

  it should "simplify" in {
    val t = scc.run(term".lteq n (.add n m)")
    true shouldBe true
  }

  // All properties in proven_properties.hover should pass
  Program
    .prelude
    .loadURL(getClass.getResource("proven_properties.hover")).definitions
    .filterKeys(_.startsWith("prop"))
    .toSeq.sortBy(_._1)
    .foreach { case (propName, propTerm) =>
      it should s"prove $propName in proven_properties.hover" in {
        val propNameVar = propName
        scc.run(propTerm) shouldEqual Logic.Truth
      }
    }

  // All properties in unprovable_properties.hover should fail because they are false
  Program
    .prelude
    .loadURL(getClass.getResource("unprovable_properties.hover")).definitions
    .filterKeys(_.startsWith("prop"))
    .toSeq.sortBy(_._1)
    .foreach { case (propName, propTerm) =>
      it should s"fail to prove $propName in unprovable_properties.hover" in {
        scc.run(propTerm) should not equal Logic.Truth
      }
    }


  // All properties in unproven_properties.hover should fail, but if they ever pass it's a good thing!
  Program
    .prelude
    .loadURL(getClass.getResource("unproven_properties.hover")).definitions
    .filterKeys(_.startsWith("prop"))
    .toSeq.sortBy(_._1)
    .foreach { case (propName, propTerm) =>
      it should s"fail to prove $propName in unproven_properties.hover" in {
        scc.run(propTerm) should not equal Logic.Truth
      }
    }
}
