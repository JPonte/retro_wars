package org.jponte

import io.circe.*
import io.circe.generic.auto.*
import io.circe.syntax.*
import monocle.syntax.all.*

case class Deployment(
    unit: Character,
    player: Int,
    health: Int = 100,
    canMove: Boolean = true,
    hasAction: Boolean = true
)

sealed trait CityStatus
case class Captured(player: Int) extends CityStatus
case class Capturing(player: Int, health: Int = 20) extends CityStatus

case class GameState(
    tileMap: TileMap,
    units: Map[Position, Deployment],
    cities: Map[Position, CityStatus],
    players: Seq[Player],
    currentPlayer: Int
) {
  def runAction(action: GameAction): Either[String, GameState] = action match {
    case Move(from, target) =>
      val deployment = units.get(from)
      val targetDeployment = units.get(target)
      (deployment, targetDeployment) match {
        case (None, _) => Left("No unit in this position")
        case (_, Some(_)) => Left("Target position is occupied")
        case (Some(d), _) if d.player != currentPlayer =>
          Left("Unit doesn't belong to the current player")
        case (Some(d), _) if !d.canMove => Left("Unit already moved this turn")
        case (Some(d), None) =>
          val newState = this
            .focus(_.units)
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
        case (Some(d), _) if d.player != currentPlayer =>
          Left("Unit doesn't belong to the current player")
        case (Some(d), _) if !d.hasAction =>
          Left("Unit already acted this turn")
        case (_, Some(d)) if d.player == currentPlayer =>
          Left("Target unit is allied")
        case (Some(d), Some(_))
            if !Utils.inGunRange(from, 1, d.unit.attackRange, tileMap).contains(target) =>
          Left("Target not in range")
        case (Some(d1), Some(d2)) =>
          val newState = this
            .focus(_.units)
            .at(target)
            .replace(Some(d2.copy(health = d2.health - d1.unit.baseAttack)).filter(_.health > 0))
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
    case PurchaseUnit(from, character) =>
      val tile = tileMap.tileAt(from)
      val deployment = units.get(from)
      (tile, deployment) match {
        case (_, Some(d)) if d.player != currentPlayer =>
          Left("Unit doesn't belong to the current player")
        case (Some(tile), None) if tile == Tile.Factory =>
          val newState = this
            .focus(_.units)
            .at(from)
            .modify(_ =>
              Some(Deployment(character, currentPlayer, canMove = false, hasAction = false))
            )
          Right(newState)
        case _ => Left("Invalid move")
      }
    case CaptureCity(from) =>
      val tile = tileMap.tileAt(from).filter(Tile.cities.contains)
      val deployment = units.get(from).filter(d => Character.infantryCharacters.contains(d.unit))
      val cityStatus = cities.get(from)
      (tile, deployment, cityStatus) match {
        case (_, Some(d), _) if d.player != currentPlayer =>
          Left("Unit doesn't belong to the current player")
        case (_, Some(d), _) if !d.hasAction =>
          Left("Unit already acted this turn")
        case (None, _, _) =>
          Left("Tile isn't capturable")
        case (Some(t), Some(d), Some(Captured(player))) if d.player == player =>
          Left("City already captured")
        case (Some(t), Some(d), Some(Capturing(player, health))) if d.player == player =>
          val newCityStatus = Capturing(player, health - d.health)
          val newState = if (newCityStatus.health <= 0) {
            this.focus(_.cities).at(from).replace(Some(Captured(player)))
          } else {
            this.focus(_.cities).at(from).replace(Some(newCityStatus))
          }
          Right(newState)
        case (Some(t), Some(d), None) =>
          val newCityStatus = Capturing(d.player, health = 20 - (d.health / 10))
          val newState = this.focus(_.cities).at(from).replace(Some(newCityStatus))
          Right(newState)
        case _ => Left("Invalid move")
      }
  }

  def show: String =
    (0 until tileMap.height)
      .map { y =>
        (0 until tileMap.width)
          .map { x =>
            val pos = Position(x, y)
            tileMap
              .tileAt(pos)
              .map(_.symbol)
              .getOrElse(" ") + units.get(pos).map(_.unit.symbol.toString).getOrElse(" ")
          }
          .mkString(" ")
      }
      .mkString("\n")
}

object GameState {
  implicit val encodeGameState: Encoder[GameState] = (gs: GameState) =>
    Json.obj(
      ("tileMap", gs.tileMap.asJson),
      ("units", gs.units.toSeq.asJson),
      ("cities", gs.cities.toSeq.asJson),
      ("players", gs.players.toSeq.asJson),
      ("currentPlayer", Json.fromInt(gs.currentPlayer))
    )

  implicit val decodeGameState: Decoder[GameState] = (c: HCursor) =>
    for {
      tiles <- c.downField("tileMap").as[TileMap]
      units <- c.downField("units").as[Seq[(Position, Deployment)]]
      cities <- c.downField("cities").as[Seq[(Position, CityStatus)]]
      players <- c.downField("players").as[Seq[Player]]
      currentPlayer <- c.downField("currentPlayer").as[Int]
    } yield GameState(tiles, units.toMap, cities.toMap, players, currentPlayer)

}
