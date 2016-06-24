package hoverboard

import hoverboard.term._
import org.scalatest._

class ParserTest extends FlatSpec with Matchers {
  implicit val program = Program.empty

  "term parsing" should "work" in {
    t"(fn x x -> f x) y z" shouldBe Lam("x", Lam("x", Var("f")(Var("x"))))(Var("y"), Var("z"))
  }
}
