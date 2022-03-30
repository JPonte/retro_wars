package org.jponte

import indigo.*
import Tile._

case class NeighbourTiles(north: Option[Tile], south: Option[Tile], west: Option[Tile], east: Option[Tile])

object TileAssets {

  val tileSetAssetName: AssetName = AssetName("tileset")
  val tileSet: AssetType.Image = AssetType.Image(tileSetAssetName, AssetPath("./assets/tiles_packed.png"))

  def getTileGraphic(position: Position, tileMap: TileMap): Graphic[Material.Bitmap] = {
    tileMap.tileAt(position).map { tile =>
      val neighbours = getNeighbourTiles(position, tileMap)
      val tilePos = tile match {
        case Road => roadTile(neighbours)
        case Grass => (2, 4)
        case Sea => (6, 3)
        case Forest => (0, 5)
        case City => (0, 6)
        case Factory | Airport | Port => (0, 7)
        case Headquarters => (0, 8)
        case _ => (10, 0)
      }
      Graphic(Rectangle(tilePos._1 * 16, tilePos._2 * 16, 16, 16), 3, Material.Bitmap(tileSetAssetName))
    }.getOrElse(
      Graphic(Rectangle(0, 0, 16, 16), 3, Material.Bitmap(tileSetAssetName))
    )
  }

  private def roadTile(neighbourTiles: NeighbourTiles): (Int, Int) = neighbourTiles match {
    case NeighbourTiles(Some(Road), Some(Road), Some(Road), Some(Road)) => (2, 7)

    case NeighbourTiles(n, Some(Road), Some(Road), Some(Road)) => (4, 7)
    case NeighbourTiles(Some(Road), s, Some(Road), Some(Road)) => (5, 7)
    case NeighbourTiles(Some(Road), Some(Road), w, Some(Road)) => (4, 6)
    case NeighbourTiles(Some(Road), Some(Road), Some(Road), e) => (5, 6)

    case NeighbourTiles(Some(Road), Some(Road), w, e) => (1, 7)
    case NeighbourTiles(Some(Road), s, Some(Road), e) => (3, 8)
    case NeighbourTiles(Some(Road), s, w, Some(Road)) => (1, 8)

    case NeighbourTiles(n, Some(Road), Some(Road), e) => (3, 6)
    case NeighbourTiles(n, Some(Road), w, Some(Road)) => (1, 6)

    case NeighbourTiles(n, s, Some(Road), Some(Road)) => (2, 6)

    case NeighbourTiles(Some(Road), s, w, e) => (4, 9)
    case NeighbourTiles(n, Some(Road), w, e) => (4, 8)
    case NeighbourTiles(n, s, Some(Road), e) => (5, 9)
    case NeighbourTiles(n, s, w, Some(Road)) => (5, 8)

    case _ => (1, 7)
  }
  
  def getNeighbourTiles(position: Position, tileMap: TileMap): NeighbourTiles =
    NeighbourTiles(
      tileMap.tileAt(Position(position.x, position.y - 1)),
      tileMap.tileAt(Position(position.x, position.y + 1)),
      tileMap.tileAt(Position(position.x - 1, position.y)),
      tileMap.tileAt(Position(position.x + 1, position.y)))

}
