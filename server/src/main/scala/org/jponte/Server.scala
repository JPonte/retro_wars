package org.jponte

import cats.effect.*
import cats.effect.std.Queue
import fs2.{Pipe, Stream}
import fs2.concurrent.{SignallingRef, Topic}
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
      actions <- Topic[IO, GameAction]
      _ <- new WebSocketApp(gameState, actions).stream.compile.drain
    } yield ExitCode.Success
  }
}

class WebSocketApp(
    gameState: Ref[IO, GameState],
    actions: Topic[IO, GameAction]
) extends Http4sDsl[IO] {
  def routes(wsb: WebSocketBuilder[IO]): HttpRoutes[IO] =
    HttpRoutes.of[IO] {
      case GET -> Root =>
        val firstMessage: Stream[IO, WebSocketFrame] = Stream
          .eval(gameState.get)
          .map { gs =>
            GameStateWebSocketMessage(gs).asInstanceOf[WebSocketMessage].asJson.noSpaces
          }
          .map(Text(_))

        val sendStream: Stream[IO, WebSocketFrame] = firstMessage ++ actions
          .subscribe(10)
          .evalMap(_ => gameState.get)
          .map(GameStateWebSocketMessage.apply)
          .map(_.asInstanceOf[WebSocketMessage].asJson.noSpaces)
          .map(Text(_))

        val receivePipe: Pipe[IO, WebSocketFrame, Unit] =
          _.collect { case Text(str, _) => decode[WebSocketMessage](str) }
            .collect { case Right(GameActionWebSocketMessage(gameAction)) => gameAction }
            .evalTap(action => gameState.update(gs => gs.runAction(action).getOrElse(gs)))
            .through(actions.publish)
        wsb.build(sendStream, receivePipe)
    }

  def stream: Stream[IO, ExitCode] =
    BlazeServerBuilder[IO]
      .bindHttp(8080)
      .withHttpWebSocketApp(wsb =>
        Router(
          "/" -> fileService[IO](FileService.Config("./game/target/indigoBuild")),
          "ws" -> routes(wsb)
        ).orNotFound
      )
      .serve
}
