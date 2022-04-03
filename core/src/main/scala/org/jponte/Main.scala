package org.jponte

import io.circe.*
import io.circe.generic.auto.*
import io.circe.parser.*
import io.circe.syntax.*

object Main {
  def main(args: Array[String]): Unit = {
    val gameState = Utils.testMap
    println(gameState.asJson.noSpaces)

    val action = PurchaseUnit(Position(1, 5), Character.Tank)
    val newState = gameState.runAction(action)
    println(newState.map(_.units))
    println(GameActionWebSocketMessage(action).asInstanceOf[WebSocketMessage].asJson.noSpaces)

  }
}
