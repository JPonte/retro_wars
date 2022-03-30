package org.jponte

import indigo.shared.events.GlobalEvent

object GlobalEvents {

  case object EndTurnEvent extends GlobalEvent

  case class MoveEvent(from: Position, to: Position) extends GlobalEvent

  case class AttackEvent(from: Position, to: Position) extends GlobalEvent

  case object AttackActionEvent extends GlobalEvent

  case object WaitActionEvent extends GlobalEvent

  case object CancelActionEvent extends GlobalEvent

}
