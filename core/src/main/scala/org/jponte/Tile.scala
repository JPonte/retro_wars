package org.jponte

import org.jponte.Tile.MovementCost
import MovementType.*
import io.circe.*
import io.circe.generic.auto.*
import io.circe.syntax.*

case class Tile(
    name: String,
    symbol: String,
    defense: Int,
    moveCost: MovementCost
)

object Tile {
  type MovementCost = Map[MovementType, Int]

  val Grass: Tile = Tile(
    "Grass",
    "#",
    1,
    Map(Foot -> 1, Boots -> 1, Tires -> 2, Tread -> 1, Air -> 1)
  )
  val Road: Tile = Tile(
    "Road",
    "=",
    0,
    Map(Foot -> 1, Boots -> 1, Tires -> 1, Tread -> 1, Air -> 1)
  )
  val Forest: Tile = Tile(
    "Forest",
    "*",
    2,
    Map(Foot -> 1, Boots -> 1, Tires -> 3, Tread -> 2, Air -> 1)
  )
  val Sea: Tile = Tile("Sea", "~", 0, Map(Air -> 1, MovementType.Sea -> 1))

  val City: Tile = Tile(
    "City",
    "X",
    3,
    Map(Foot -> 1, Boots -> 1, Tires -> 1, Tread -> 1, Air -> 1)
  )
  val Factory: Tile = Tile(
    "Factory",
    "F",
    3,
    Map(Foot -> 1, Boots -> 1, Tires -> 1, Tread -> 1, Air -> 1)
  )
  val Airport: Tile = Tile(
    "Airport",
    "A",
    3,
    Map(Foot -> 1, Boots -> 1, Tires -> 1, Tread -> 1, Air -> 1)
  )
  val Port: Tile = Tile(
    "Port",
    "P",
    3,
    Map(
      Foot -> 1,
      Boots -> 1,
      Tires -> 1,
      Tread -> 1,
      Air -> 1,
      MovementType.Sea -> 1
    )
  )
  val Headquarters: Tile = Tile(
    "HQ",
    "H",
    4,
    Map(
      Foot -> 1,
      Boots -> 1,
      Tires -> 1,
      Tread -> 1,
      Air -> 1,
      MovementType.Sea -> 1
    )
  )

  val allTiles: Seq[Tile] =
    Seq(Grass, Road, Forest, Sea, City, Factory, Airport, Port, Headquarters)
  val cities: Set[Tile] = Set(City, Factory, Airport, Port, Headquarters)

  implicit val encodeMovementCost: Encoder[MovementCost] = (mc: MovementCost) =>
    mc.toList.asJson

  implicit val decodeMovementCost: Decoder[MovementCost] = (c: HCursor) =>
    c.as[List[(MovementType, Int)]].map(_.toMap)

}
