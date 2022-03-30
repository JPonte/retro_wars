package org.jponte

import cats.effect.*
import cats.effect.std.Queue
import fs2.{Pipe, Stream}
import io.circe.*
import io.circe.generic.auto.*
import io.circe.parser.*
import io.circe.syntax.*
import org.http4s.*
import org.http4s.blaze.server.BlazeServerBuilder
import org.http4s.dsl.Http4sDsl
import org.http4s.implicits.*
import org.http4s.server.Router
import org.http4s.server.staticcontent.{FileService, fileService}
import org.http4s.server.websocket.*
import org.http4s.websocket.WebSocketFrame
import org.http4s.websocket.WebSocketFrame.*

import scala.concurrent.duration.*
import scala.util.Random

object Server extends IOApp {
  override def run(args: List[String]): IO[ExitCode] = {
    for {
      gameState <- IO.ref(Utils.testMap)
      _ <- new WebSocketApp(gameState).stream.compile.drain
    } yield ExitCode.Success
  }
}

class WebSocketApp(
    positions: Ref[IO, GameState]
) extends Http4sDsl[IO] {
  def routes(wsb: WebSocketBuilder[IO]): HttpRoutes[IO] =
    HttpRoutes.of[IO] {
      case GET -> Root =>
        val echoReply: Pipe[IO, WebSocketFrame, WebSocketFrame] =
          _.collect {
            case Text(msg, _) => Text("You sent the server: " + msg)
            case _ => Text("Something new")
          }

        Queue.unbounded[IO, Option[WebSocketFrame]].flatMap { q =>
          val d: Stream[IO, WebSocketFrame] = Stream.fromQueueNoneTerminated(q).through(echoReply)
          val e: Pipe[IO, WebSocketFrame, Unit] = _.enqueueNoneTerminated(q)
          wsb.build(d, e)
        }
    }

  def stream: Stream[IO, ExitCode] =
    BlazeServerBuilder[IO]
      .bindHttp(8080)
      .withHttpWebSocketApp(wsb =>
        Router(
          "/" -> fileService[IO](FileService.Config("./client")),
          "ws" -> routes(wsb)
        ).orNotFound
      )
      .serve
}
