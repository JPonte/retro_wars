package org.jponte

import io.circe._, io.circe.generic.auto._, io.circe.parser._, io.circe.syntax._
import monocle.syntax.all._
import GameState._
import scala.annotation.tailrec

case class Deployment(unit: Character, player: Int, health: Int = 100, canMove: Boolean = true, hasAction: Boolean = true)

case class GameState(tileMap: TileMap, units: Map[Position, Deployment], players: Seq[Player], currentPlayer: Int) {
  def runAction(action: Action): Either[String, GameState] = action match {
    case Move(from, target) =>
      val deployment = units.get(from)
      val targetDeployment = units.get(target)
      (deployment, targetDeployment) match {
        case (None, _) => Left("No unit in this position")
        case (_, Some(_)) => Left("Target position is occupied")
        case (Some(Deployment(_, p, _, _, _)), _) if p != currentPlayer => Left("Unit doesn't belong to the current player")
        case (Some(Deployment(_, _, _, false, _)), _) => Left("Unit already moved this turn")
        case (Some(d), None) =>
          val newState = this.focus(_.units)
            .at(from)
            .replace(None)
            .focus(_.units)
            .at(target)
            .replace(Some(d.copy(canMove = false)))
          Right(newState)
      }
    case Attack(from, target) =>
      val deployment = units.get(from)
      val targetDeployment = units.get(target)
      (deployment, targetDeployment) match {
        case (None, _) => Left("No unit in this position")
        case (_, None) => Left("Target position has no one")
        case (Some(Deployment(_, p, _, _, _)), _) if p != currentPlayer => Left("Unit doesn't belong to the current player")
        case (Some(Deployment(_, _, _, _, false)), _) => Left("Unit already moved this turn")
        case (_, Some(Deployment(_, p, _, _, _))) if p == currentPlayer => Left("Target unit is allied")
        case (Some(Deployment(c1, _, _, _, _)), Some(_)) if !ranges(tileMap, from).filter(c => c._2 > 0 && c._2 <= c1.range).contains(target) =>
          Left("Target not in range")
        case (Some(Deployment(c1, p1, h1, _, _)), Some(Deployment(c2, p2, h2, _, _))) =>
          val newState = this
            .focus(_.units)
            .at(target)
            .modify(_.map(_.copy(health = h2 - c1.baseAttack)).filter(_.health > 0))
            .focus(_.units)
            .at(from)
            .modify(_.map(_.copy(canMove = false, hasAction = false)))
          Right(newState)
      }
    case EndTurn =>
      val newState = this
        .focus(_.units)
        .each
        .modify(_.copy(canMove = true, hasAction = true))
        .focus(_.currentPlayer)
        .modify(currentPlayer => (currentPlayer + 1) % players.size)
      Right(newState)
  }

  def show: String =
    (0 until tileMap.height).map { y =>
      (0 until tileMap.width).map { x =>
        val pos = Position(x, y)
        tileMap.tileAt(pos).map(_.symbol).getOrElse(" ") + units.get(pos).map(_.unit.symbol.toString).getOrElse(" ")
      }.mkString(" ")
    }.mkString("\n")
}

object GameState {
  implicit val encodeGameState: Encoder[GameState] = (gs: GameState) => Json.obj(
    ("tileMap", gs.tileMap.asJson),
    ("units", gs.units.toSeq.asJson),
    ("players", gs.players.toSeq.asJson),
    ("currentPlayer", Json.fromInt(gs.currentPlayer)),
  )

  implicit val decodeGameState: Decoder[GameState] = (c: HCursor) =>
    for {
      tiles <- c.downField("tileMap").as[TileMap]
      units <- c.downField("units").as[Seq[(Position, Deployment)]]
      players <- c.downField("players").as[Seq[Player]]
      currentPlayer <- c.downField("currentPlayer").as[Int]
    } yield GameState(tiles, units.toMap, players, currentPlayer)

  //TODO: move out of here
  def ranges(tileMap: TileMap, from: Position): Map[Position, Int] = {

    @tailrec
    def scanPositions(positions: Seq[Position], costs: Map[Position, Int]): Map[Position, Int] = {

      positions.headOption match {
        case Some(pos@Position(x, y)) =>
          val currentCost = costs(pos)
          val newCost = currentCost + 1
          val nextCosts = Seq(Position(x + 1, y), Position(x - 1, y), Position(x, y + 1), Position(x, y - 1))
            .collect {
              case pos if tileMap.map.keys.exists(_ == pos) && costs.get(pos).forall(_ > newCost) =>
                pos -> costs.get(pos).fold(newCost)(oldCost => Math.min(newCost, oldCost))
            }.toMap
          scanPositions(positions.tail ++ nextCosts.keys.toSeq, costs ++ nextCosts)
        case None => costs
      }
    }

    scanPositions(Seq(from), Map(from -> 0))
  }

}

