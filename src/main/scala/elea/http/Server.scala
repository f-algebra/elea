package elea.http

import elea.{Parser, Program}
import elea.rewrite.Simplifier
import org.http4s._
import org.http4s.dsl._
import scodec.bits.ByteVector

object Server {

  val supercompiler = Simplifier.supercompilation

  val service = HttpService {
    case request @ GET -> Root / "supercompile" =>
      request.decode[ByteVector] { rawBody =>
        rawBody.decodeUtf8 match {
          case Left(err) =>
            BadRequest(err.toString)
          case Right(bodyText) =>
            Parser.parseAll(bodyText)(_.modifyTerm(supercompiler.run(_)))(Program.empty)
            Ok("spleg")
        }
      }
  }

  def start(host: String, port: Int): Unit = {

  }
}
