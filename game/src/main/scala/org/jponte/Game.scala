package org.jponte

import indigo.*
import indigoextras.subsystems.FPSCounter
import indigoextras.ui.{Button, ButtonAssets}

import scala.scalajs.js.annotation.JSExportTopLevel

case class UIState(hoverTile: Option[(Int, Int)], button: Button)

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
      .withViewport(528, 384)
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

  case object LaunchTheRocket extends GlobalEvent


  override def setup(bootData: Unit, assetCollection: AssetCollection, dice: Dice): Outcome[Startup[Unit]] =
    Outcome(Startup.Success(()))

  override def initialViewModel(startupData: Unit, model: GameState): Outcome[UIState] = Outcome(UIState(None, Button(
    buttonAssets = buttonAssets,
    bounds = Rectangle(8, 8, 32, 16),
    depth = Depth(1)
  ).withUpActions(LaunchTheRocket)))

  override def initialModel(startupData: Unit): Outcome[GameState] =
    Outcome(Utils.getRandomState(11, 8, 1))

  override def updateModel(context: FrameContext[Unit], model: GameState): GlobalEvent => Outcome[GameState] = {
    case LaunchTheRocket =>
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
      Outcome(viewModel.copy(hoverTile = Some((x / 16, y / 16))))
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

    val hover = viewModel.hoverTile.map { case (x, y) =>
      Graphic(Rectangle(3 * 16, 2 * 16, 16, 16), 2, Material.Bitmap(tileSetAssetName)).moveTo(Point(x * 16, y * 16))
    }.toList
    val units = model.units.map { case (Position(x, y), u) =>
      Graphic(Rectangle(3 * 16, 0, 16, 16), 3, Material.Bitmap(tileSetAssetName)).moveTo(Point(x * 16, y * 16))
    }.toList

    Outcome(SceneUpdateFragment(tiles ++ hover ++ units ++ List(viewModel.button.draw)))
  }
}
