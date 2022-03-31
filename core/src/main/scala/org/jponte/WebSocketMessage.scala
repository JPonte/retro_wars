package org.jponte

import io.circe.*
import io.circe.generic.semiauto.*

sealed trait WebSocketMessage

case class GameActionWebSocketMessage(gameAction: GameAction) extends WebSocketMessage


object WebSocketMessage {
  implicit val webSocketMessageDecoder: Decoder[WebSocketMessage] = deriveDecoder[WebSocketMessage]
  implicit val webSocketMessageEncoder: Encoder[WebSocketMessage] = deriveEncoder[WebSocketMessage]
}