package org.jponte

sealed trait Action

case class Move(from: Position, target: Position) extends Action

case class Attack(from: Position, target: Position) extends Action

case object EndTurn extends Action

case class PurchaseUnit(from: Position, character: Character) extends Action

case class CaptureCity(from: Position) extends Action