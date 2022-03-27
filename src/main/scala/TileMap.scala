package org.jponte

import io.circe._
import io.circe.generic.auto._
import io.circe.syntax._

case class Position(x: Int, y: Int)

object Position {
  implicit val encodePosition: Encoder[Position] = (pos: Position) => (pos.x, pos.y).asJson
  implicit val decodePosition: Decoder[Position] = (c: HCursor) => c.as[(Int, Int)].map {
    case (x, y) => Position(x, y)
  }
}

case class TileMap(width: Int, height: Int, tileSet: Seq[Tile], map: Map[Position, Int]) {
  def tileAt(position: Position): Option[Tile] =
    map.get(position).map(tileSet)
}

object TileMap {

  implicit val encodeTileMap: Encoder[TileMap] = (tm: TileMap) => Json.obj(
    ("width", Json.fromInt(tm.width)),
    ("height", Json.fromInt(tm.height)),
    ("tileSet", tm.tileSet.asJson),
    ("map", tm.map.toSeq.asJson),
  )

  implicit val decodeTileMap: Decoder[TileMap] = (c: HCursor) => for {
    width <- c.downField("width").as[Int]
    height <- c.downField("height").as[Int]
    tileSet <- c.downField("tileSet").as[Seq[Tile]]
    map <- c.downField("map").as[Seq[(Position, Int)]]
  } yield TileMap(width, height, tileSet, map.toMap)

}