package org.jponte

import indigo.*
import Tile.*
import indigo.shared.scenegraph.SpatialModifiers

case class NeighbourTiles(
    north: Option[Tile],
    south: Option[Tile],
    west: Option[Tile],
    east: Option[Tile]
)

object TileAssets {

  val BackgroundDepth: Depth = Depth(5)
  val MapTileDepth: Depth = Depth(4)
  val UnitDepth: Depth = Depth(3)
  val OverlayTilesDepth: Depth = Depth(2)
  val MovementArrowDepth: Depth = Depth(1)
  val HoverTileDepth: Depth = Depth(0)

  val tileSetAssetName: AssetName = AssetName("tileset")
  val tileSet: AssetType.Image =
    AssetType.Image(tileSetAssetName, AssetPath("./assets/tileset.png"))

  private val seaAnimationKey: AnimationKey = AnimationKey("sea_anim")
  val seaAnimation: Animation = Animation(
    seaAnimationKey,
    Frame(Rectangle(0 * 16, 24 * 16, 16, 16), Millis(500)),
    Frame(Rectangle(1 * 16, 24 * 16, 16, 16), Millis(500)),
    Frame(Rectangle(2 * 16, 24 * 16, 16, 16), Millis(500)),
    Frame(Rectangle(3 * 16, 24 * 16, 16, 16), Millis(500))
  )

  def drawMap(gameState: GameState): List[SceneNode] = {
    val tilesBackground = List(
      Shape
        .Box(
          Rectangle(Point(0, 0), Size(15 * 16, 10 * 16)),
          Fill.Color(RGBA(0.753, 0.878, 0.188, 1.0))
        )
        .withDepth(BackgroundDepth)
    )

    tilesBackground ++ gameState
      .tileMap
      .map
      .collect {
        case (position, tileId) if gameState.tileMap.tileSet.size > tileId =>
          position -> gameState.tileMap.tileSet(tileId)
      }
      .toList
      .flatMap {
        case (position, tile) =>
          val cityStatus = gameState.cities.get(position)
          val neighbours = getNeighbourTiles(position, gameState.tileMap)

          if (tile == Sea) {
            List(
              Sprite(
                BindingKey("sea animation"),
                seaAnimationKey,
                Material.Bitmap(tileSetAssetName)
              ).play().moveTo(position.x * 16, position.y * 16).withDepth(MapTileDepth)
            )
          } else {
            val tilePos = tile match {
              case Road => roadTile(neighbours)
              case Grass => (3, 0)
              case Forest => (4, 0)
              case City => (1, 2)
              case Factory | Airport | Port => (0, 2)
              case Headquarters =>
                cityStatus.flatMap(_.owner) match {
                  case Some(0) => (4, 4)
                  case Some(1) => (2, 4)
                  case Some(2) => (5, 4)
                  case Some(3) => (3, 4)
                  case _ => (1, 4)
                }
              case _ => (0, 0)
            }
            val overlayFlagTilePos = tile match {
              case City | Factory | Airport | Port | Headquarters =>
                cityStatus.flatMap(_.owner) match {
                  case Some(0) => Some((5, 21))
                  case Some(1) => Some((4, 21))
                  case Some(2) => Some((5, 20))
                  case Some(3) => Some((6, 20))
                  case _ => None
                }
              case _ => None
            }
            val siegeTilePos = tile match {
              case City | Factory | Airport | Port | Headquarters
                  if cityStatus.exists(_.underSiege.isDefined) =>
                Some(1, 20)
              case _ => None
            }

            List(
              Graphic(
                Rectangle(tilePos._1 * 16, tilePos._2 * 16, 16, 16),
                Material.Bitmap(tileSetAssetName)
              ).moveTo(position.x * 16, position.y * 16).withDepth(MapTileDepth)
            ) ++
              overlayFlagTilePos.map {
                case (x, y) =>
                  Graphic(
                    Rectangle(x * 16, y * 16, 16, 16),
                    Material.Bitmap(tileSetAssetName)
                  ).moveTo(position.x * 16, position.y * 16).withDepth(OverlayTilesDepth)
              }.toList ++
              siegeTilePos.map {
                case (x, y) =>
                  Graphic(
                    Rectangle(x * 16, y * 16, 16, 16),
                    Material.Bitmap(tileSetAssetName)
                  ).moveTo(position.x * 16, position.y * 16).withDepth(OverlayTilesDepth)
              }.toList

          }
      }
  }

  def drawDeployment(
      position: Position,
      deployment: Deployment,
      temporaryPosition: Boolean = false
  ): List[SceneNode] = {
    val tilePos = deployment.unit match {
      case Character.Infantry => (2, 16 + deployment.player)
      case Character.Tank => (3, 16 + deployment.player)
      case Character.Artillery => (0, 16 + deployment.player)
      case Character.Rockets => (4, 16 + deployment.player)
      case _ => (2, 16 + deployment.player)
    }
    val healthTilePos = deployment.health / 10 match {
      case 1 => Option((0, 21))
      case 2 => Option((1, 21))
      case 3 => Option((2, 21))
      case 4 => Option((0, 22))
      case 5 => Option((1, 22))
      case 6 => Option((2, 22))
      case 7 => Option((0, 23))
      case 8 => Option((1, 23))
      case 9 => Option((2, 23))
      case _ => None
    }

    val alpha = if (!temporaryPosition) 1.0 else 0.75
    val tint = if (deployment.canMove) RGB.White else RGB.SlateGray

    val characterGraphic = Graphic(
      Rectangle(tilePos._1 * 16, tilePos._2 * 16, 16, 16),
      Material.ImageEffects(tileSetAssetName).withAlpha(alpha).withTint(tint)
    ).moveTo(position.x * 16, position.y * 16).withDepth(UnitDepth)

    val healthGraphic = healthTilePos
      .map {
        case (x, y) =>
          Graphic(
            Rectangle(x * 16, y * 16, 16, 16),
            Material.Bitmap(tileSetAssetName)
          ).moveTo(position.x * 16, position.y * 16).withDepth(UnitDepth)
      }
      .toList
      .filter(_ => !temporaryPosition)

    List(characterGraphic) ++ healthGraphic
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

  private def getNeighbourTiles(position: Position, tileMap: TileMap): NeighbourTiles =
    NeighbourTiles(
      tileMap.tileAt(Position(position.x, position.y - 1)),
      tileMap.tileAt(Position(position.x, position.y + 1)),
      tileMap.tileAt(Position(position.x - 1, position.y)),
      tileMap.tileAt(Position(position.x + 1, position.y))
    )

}
