package org.jponte

import org.jponte.{Character, Position}
import io.circe._, io.circe.generic.semiauto._

sealed trait GameAction

case class Move(from: Position, target: Position) extends GameAction

case class Attack(from: Position, target: Position) extends GameAction

case object EndTurn extends GameAction

case class PurchaseUnit(from: Position, character: Character) extends GameAction

case class CaptureCity(from: Position) extends GameAction

object GameAction {
  implicit val gameActionDecoder: Decoder[GameAction] = deriveDecoder[GameAction]
  implicit val gameActionEncoder: Encoder[GameAction] = deriveEncoder[GameAction]
}
