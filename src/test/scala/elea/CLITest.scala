package elea

import java.io.{ByteArrayOutputStream, PrintStream}

import org.scalatest.{FlatSpec, Matchers}

class CLITest extends FlatSpec with Matchers {

  "reading from a file" should "correctly output supercompiled definitions" in {
    val testProgram =
      """
        |(defdata Nat
        |    (Zero)
        |    (Suc Nat))
        |(defun add n m
        |    (match n
        |        (Zero -> m)
        |        (Suc n' -> Suc (add n' m))))
      """.stripMargin

    val expectedOutput =
      """
        |(defdata Nat
        |     (Zero)
        |     (Suc Nat))
        |(defun add n m
        |   (fix add n
        |     (match n
        |       (Zero -> m)
        |       (Suc n' -> Suc (add n')))) n)
      """.stripMargin

    val stdout = new ByteArrayOutputStream
    Console.withOut(stdout) {
      CLI.run(CLI.Config(), testProgram)
    }
    val output = stdout.toString
    output shouldEqual expectedOutput
  }
}
