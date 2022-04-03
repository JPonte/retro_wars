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

case class Siege(player: Int, remaining: Int = 20)
case class CityStatus(owner: Option[Int], underSiege: Option[Siege])

case class GameState(
    tileMap: TileMap,
    units: Map[Position, Deployment],
    cities: Map[Position, CityStatus],
    players: List[Player],
    currentPlayer: Int
) {

  def getCurrentPlayer: Player = players(currentPlayer)

  def runAction(action: GameAction): Either[String, GameState] = action match {
    case Move(from, target) =>
      val deployment = units.get(from)
      val targetDeployment = units.get(target)
      (deployment, targetDeployment) match {
        case (None, _) => Left("No unit in this position")
        case (Some(d), _) if d.player != currentPlayer =>
          Left("Unit doesn't belong to the current player")
        case (Some(_), Some(_)) if from == target => Right(this)
        case (_, Some(_)) => Left("Target position is occupied")
        case (Some(d), _) if !d.canMove => Left("Unit already moved this turn")
        case (Some(d), None) =>
          val newState = this
            .focus(_.units)
            .at(from)
            .replace(None)
            .focus(_.units)
            .at(target)
            .replace(Some(d.copy(canMove = false, hasAction = d.unit.hasActionAfterMove)))
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
            if !Utils
              .inGunRange(from, d.unit.minAttackRange, d.unit.maxAttackRange, tileMap)
              .contains(target) =>
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
      val nextPlayer = (currentPlayer + 1) % players.size
      val nextPlayerIncome = cities.count(_._2.owner.contains(nextPlayer)) * 1000
      val newState = this
        .focus(_.units)
        .each
        .modify(_.copy(canMove = true, hasAction = true))
        .focus(_.currentPlayer)
        .replace(nextPlayer)
        .focus(_.players.index(nextPlayer).funds)
        .modify(_ + nextPlayerIncome)
      Right(newState)
    case PurchaseUnit(from, character) =>
      val tile = tileMap.tileAt(from)
      val deployment = units.get(from)
      val cityStatus = cities.get(from)
      (tile, deployment, cityStatus) match {
        case (Some(tile), None, Some(CityStatus(Some(owner), _)))
            if tile == Tile.Factory && owner == currentPlayer && players(
              currentPlayer
            ).funds >= character.cost =>
          val newState = this
            .focus(_.units)
            .at(from)
            .replace(Some(Deployment(character, currentPlayer, canMove = false, hasAction = false)))
            .focus(_.players.index(currentPlayer).funds)
            .modify(_ - character.cost)
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
        case (Some(t), Some(d), Some(CityStatus(Some(owner), _))) if d.player == owner =>
          Left("City already captured")
        case (Some(t), Some(d), Some(CityStatus(currentOwner, Some(Siege(player, remaining)))))
            if d.player == player =>
          val newSiege = Siege(player, remaining - (d.health / 10))
          val newState = (if (newSiege.remaining <= 0) {
                            this
                              .focus(_.cities)
                              .at(from)
                              .replace(Some(CityStatus(Some(player), None)))
                          } else {
                            this
                              .focus(_.cities)
                              .at(from)
                              .replace(Some(CityStatus(currentOwner, Some(newSiege))))
                          })
            .focus(_.units)
            .at(from)
            .modify(_.map(_.copy(canMove = false, hasAction = false)))
          Right(newState)
        case (Some(t), Some(d), cs) if cs.forall(!_.owner.contains(d.player)) =>
          val newSiege = Siege(d.player, 20 - (d.health / 10))
          val newState =
            this
              .focus(_.cities)
              .at(from)
              .replace(Some(CityStatus(cs.flatMap(_.owner), Some(newSiege))))
              .focus(_.units)
              .at(from)
              .modify(_.map(_.copy(canMove = false, hasAction = false)))
          Right(newState)
        case _ => Left("Invalid move")
      }
  }
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
      players <- c.downField("players").as[List[Player]]
      currentPlayer <- c.downField("currentPlayer").as[Int]
    } yield GameState(tiles, units.toMap, cities.toMap, players, currentPlayer)

}
