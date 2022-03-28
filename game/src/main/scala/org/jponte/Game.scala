package org.jponte

import indigo.*
import indigoextras.subsystems.FPSCounter
import indigoextras.ui.{Button, ButtonAssets}

import scala.scalajs.js.annotation.JSExportTopLevel

case class UIState(hoverTile: Option[Position], selectedUnit: Option[Position], button: Button)

@JSExportTopLevel("IndigoGame")
object Game extends IndigoDemo[Unit, Unit, GameState, UIState] {

  override def eventFilters: EventFilters = EventFilters.Permissive

  private val tileSetAssetName = AssetName("tileset")
  private val magnification = 3
  private val tileAtlas: Map[Int, (Int, Int)] = Map(
    0 -> (2, 4),
    1 -> (1, 7),
    2 -> (0, 5),
    3 -> (6, 3),
  )

  override def boot(flags: Map[String, String]): Outcome[BootResult[Unit]] =
    Outcome(BootResult.noData(GameConfig.default
      .withViewport(720, 384)
      .withMagnification(magnification))
      .withAssets(
        AssetType.Image(tileSetAssetName, AssetPath("./assets/tiles_packed.png")),
        AssetType.Image(AssetName("button_up"), AssetPath("./assets/button_up.png")),
        AssetType.Image(AssetName("button_down"), AssetPath("./assets/button_down.png"))
      ))

  val buttonAssets: ButtonAssets = ButtonAssets(
    up = Graphic(48, 48, Material.Bitmap(AssetName("button_up"))).scaleBy(Vector2(2.0 / 3.0, 1.0 / 3.0)),
    over = Graphic(48, 48, Material.Bitmap(AssetName("button_up"))).scaleBy(Vector2(2.0 / 3.0, 1.0 / 3.0)),
    down = Graphic(48, 48, Material.Bitmap(AssetName("button_down"))).scaleBy(Vector2(2.0 / 3.0, 1.0 / 3.0)),
  )

  case object EndTurnEvent extends GlobalEvent

  override def setup(bootData: Unit, assetCollection: AssetCollection, dice: Dice): Outcome[Startup[Unit]] =
    Outcome(Startup.Success(()))

  override def initialViewModel(startupData: Unit, model: GameState): Outcome[UIState] =
    Outcome(
      UIState(
        None,
        None,
        Button(
          buttonAssets = buttonAssets,
          bounds = Rectangle(0, 0, 32, 16),
          depth = Depth(1)
        ).withUpActions(EndTurnEvent).moveTo(180, 96)))

  override def initialModel(startupData: Unit): Outcome[GameState] =
    Outcome(Utils.getRandomState(11, 8, 1))

  override def updateModel(context: FrameContext[Unit], model: GameState): GlobalEvent => Outcome[GameState] = {
    case EndTurnEvent =>
      model.runAction(EndTurn) match {
        case Left(error) =>
          println(error)
          Outcome(model)
        case Right(newModel) => Outcome(newModel)
      }
    case _ => Outcome(model)
  }

  override def updateViewModel(context: FrameContext[Unit], model: GameState, viewModel: UIState): GlobalEvent => Outcome[UIState] = {
    case MouseEvent.Move(Point(x, y)) =>
      Outcome(viewModel.copy(hoverTile = Some(Position(x / 16, y / 16))))
    case MouseEvent.Click(Point(x, y)) =>
      val selectedUnit = Some(Position(x / 16, y / 16)).filter(model.units.contains)
      Outcome(viewModel.copy(selectedUnit = selectedUnit))
    case FrameTick =>
      viewModel.button.update(context.inputState.mouse).map { btn =>
        viewModel.copy(button = btn)
      }
    case _ => Outcome(viewModel)
  }

  override def present(context: FrameContext[Unit], model: GameState, viewModel: UIState): Outcome[SceneUpdateFragment] = {
    val tiles = model.tileMap.map.map { case (Position(x, y), i) =>
      val tilePos = tileAtlas.getOrElse(i, (0, 6))
      Graphic(Rectangle(tilePos._1 * 16, tilePos._2 * 16, 16, 16), 3, Material.Bitmap(tileSetAssetName)).moveTo(Point(x * 16, y * 16))
    }.toList

    val hover = viewModel.hoverTile.flatMap { case p@Position(x, y) =>
      if (model.tileMap.map.contains(p))
        Some(Graphic(Rectangle(3 * 16, 2 * 16, 16, 16), 2, Material.Bitmap(tileSetAssetName)).moveTo(Point(x * 16, y * 16)))
      else
        None
    }.toList

    val units = model.units.map { case (Position(x, y), u) =>
      Graphic(Rectangle(3 * 16, 0, 16, 16), 3, Material.Bitmap(tileSetAssetName)).moveTo(Point(x * 16, y * 16))
    }.toList

    def sideMenuText(text: String, i: Int) =
      TextBox(text, 200, 16)
        .withFontFamily(FontFamily.monospace)
        .withColor(RGBA.White)
        .withFontSize(Pixels(12))
        .alignStart
        .moveTo(180, i * 16)

    val tileInfo = viewModel.hoverTile.flatMap(model.tileMap.tileAt)
      .toList
      .flatMap { tile =>
        List(
          sideMenuText(tile.name, 0),
          sideMenuText(s"Def: ${tile.defense}", 1))
      }

    val unitInfo = viewModel.hoverTile.flatMap(model.units.get).toList.flatMap { deployment =>
      List(
        sideMenuText(deployment.unit.name, 2),
        sideMenuText(s"${deployment.health}/100", 3),
        sideMenuText(s"P: ${deployment.player}", 4))
    }

    val selectedUnitRange = viewModel.selectedUnit.flatMap(p => model.units.get(p).map(p -> _)).toList.flatMap { case (position, deployment) =>
      val tiles = Utils.ranges(model.tileMap, position).filter(_._2 < deployment.unit.moveRange)
      val path = viewModel.hoverTile.toList.filter(tiles.contains).flatMap { hoverPos =>
        Utils.path(model.tileMap, position, hoverPos)
      }.map { case Position(x, y) => Point(x * 16 + 8, y * 16 + 8) }

      val pathLines = if (path.size > 1) {
        path.zip(path.tail).map { case (from, to) =>
          Shape.Line(from, to, Stroke(4, RGBA.Red))
        }
      } else {
        List()
      }

      tiles.keys.map { case Position(x, y) =>
        Shape.Box(
          Rectangle(Point(x * 16, y * 16), Size(16, 16)),
          Fill.Color(RGBA.White.withAlpha(0.4)),
          Stroke(1, RGBA.SteelBlue)
        ).withDepth(Depth(2))
      } ++ pathLines
    }

    Outcome(SceneUpdateFragment(tiles ++ hover ++ units ++ tileInfo ++ unitInfo ++ selectedUnitRange ++ List(viewModel.button.draw)))
  }
}
