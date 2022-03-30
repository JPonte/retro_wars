package org.jponte

sealed trait WebSocketMessage

case class GameActionWebSocketMessage(gameAction: GameAction) extends WebSocketMessage
