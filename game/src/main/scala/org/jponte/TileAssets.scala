package org.jponte

import indigo.*
import Tile._

case class NeighbourTiles(
    north: Option[Tile],
    south: Option[Tile],
    west: Option[Tile],
    east: Option[Tile]
)

object TileAssets {

  val tileSetAssetName: AssetName = AssetName("tileset")
  val tileSet: AssetType.Image =
    AssetType.Image(tileSetAssetName, AssetPath("./assets/tileset.png"))

  def getTileGraphic(position: Position, tileMap: TileMap): Graphic[Material.Bitmap] = {
    tileMap
      .tileAt(position)
      .map { tile =>
        val neighbours = getNeighbourTiles(position, tileMap)
        val tilePos = tile match {
          case Road => roadTile(neighbours)
          case Grass => (3, 0)
          case Sea => (0, 24)
          case Forest => (4, 0)
          case City => (2, 1)
          case Factory | Airport | Port => (0, 2)
          case Headquarters => (2, 4)
          case _ => (0, 0)
        }
        Graphic(
          Rectangle(tilePos._1 * 16, tilePos._2 * 16, 16, 16),
          3,
          Material.Bitmap(tileSetAssetName)
        )
      }
      .getOrElse(Graphic(Rectangle(0, 0, 16, 16), 3, Material.Bitmap(tileSetAssetName)))
  }

  private def roadTile(neighbourTiles: NeighbourTiles): (Int, Int) =
    neighbourTiles match {
      case NeighbourTiles(Some(Road), Some(Road), Some(Road), Some(Road)) =>
        (4, 11)

      case NeighbourTiles(n, Some(Road), Some(Road), Some(Road)) => (4, 9)
      case NeighbourTiles(Some(Road), s, Some(Road), Some(Road)) => (5, 9)
      case NeighbourTiles(Some(Road), Some(Road), w, Some(Road)) => (6, 11)
      case NeighbourTiles(Some(Road), Some(Road), Some(Road), e) => (6, 10)

      case NeighbourTiles(Some(Road), Some(Road), w, e) => (3, 11)
      case NeighbourTiles(Some(Road), s, Some(Road), e) => (5, 12)
      case NeighbourTiles(Some(Road), s, w, Some(Road)) => (3, 12)

      case NeighbourTiles(n, Some(Road), Some(Road), e) => (5, 10)
      case NeighbourTiles(n, Some(Road), w, Some(Road)) => (3, 10)

      case NeighbourTiles(n, s, Some(Road), Some(Road)) => (4, 10)

      case NeighbourTiles(Some(Road), s, w, e) => (3, 11)
      case NeighbourTiles(n, Some(Road), w, e) => (3, 11)
      case NeighbourTiles(n, s, Some(Road), e) => (4, 10)
      case NeighbourTiles(n, s, w, Some(Road)) => (4, 10)

      case _ => (4, 10)
    }

  def getNeighbourTiles(position: Position, tileMap: TileMap): NeighbourTiles =
    NeighbourTiles(
      tileMap.tileAt(Position(position.x, position.y - 1)),
      tileMap.tileAt(Position(position.x, position.y + 1)),
      tileMap.tileAt(Position(position.x - 1, position.y)),
      tileMap.tileAt(Position(position.x + 1, position.y))
    )

}
