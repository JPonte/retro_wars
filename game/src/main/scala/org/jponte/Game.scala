package org.jponte

import cats.{Align, Alternative, Applicative, CoflatMap, Eval, Foldable, Functor, Monad, Traverse}
import cats.implicits.*
import indigo.*
import indigo.shared.events.MouseButton
import indigoextras.subsystems.FPSCounter
import indigoextras.ui.{Button, ButtonAssets}
import org.jponte.GlobalEvents.*

import scala.scalajs.js.annotation.JSExportTopLevel

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

  implicit val outcomeApplicative: Applicative[Outcome] = new Applicative[Outcome] {
    override def pure[A](x: A): Outcome[A] = Outcome(x)

    override def ap[A, B](ff: Outcome[A => B])(fa: Outcome[A]): Outcome[B] = fa.flatMap(a => ff.map(f => f(a)))
  }

  override def boot(flags: Map[String, String]): Outcome[BootResult[Unit]] =
    Outcome(BootResult.noData(GameConfig.default
      .withViewport((15 + 4) * 16 * magnification, (10 * 16) * magnification)
      .withMagnification(magnification))
      .withAssets(AssetType.Image(tileSetAssetName, AssetPath("./assets/tiles_packed.png"))))

  override def setup(bootData: Unit, assetCollection: AssetCollection, dice: Dice): Outcome[Startup[Unit]] =
    Outcome(Startup.Success(()))

  override def initialViewModel(startupData: Unit, model: GameState): Outcome[UIState] =
    Outcome(OverviewState(None, UIAssets.endTurnButton, None))

  override def initialModel(startupData: Unit): Outcome[GameState] =
    Outcome(Utils.testMap)

  override def updateModel(context: FrameContext[Unit], model: GameState): GlobalEvent => Outcome[GameState] = {
    case EndTurnEvent =>
      model.runAction(EndTurn) match {
        case Left(error) =>
          println(error)
          Outcome(model)
        case Right(newModel) => Outcome(newModel)
      }
    case MoveEvent(from, to) =>
      model.runAction(Move(from, to)) match {
        case Left(error) =>
          println(error)
          Outcome(model)
        case Right(newModel) => Outcome(newModel)
      }
    case AttackEvent(from, to) =>
      model.runAction(Attack(from, to)) match {
        case Left(error) =>
          println(error)
          Outcome(model)
        case Right(newModel) => Outcome(newModel)
      }
    case _ => Outcome(model)
  }

  override def updateViewModel(context: FrameContext[Unit], model: GameState, viewModel: UIState): GlobalEvent => Outcome[UIState] = {
    case MouseEvent.Move(Point(x, y)) =>
      val tile = Option(Position(x / 16, y / 16)).filter(model.tileMap.map.contains)
      val newVm = viewModel match {
        case s: OverviewState => s.copy(hoverTile = tile)
        case s@UnitMoveState(_, selectedPosition, _, _) =>
          val path = tile.toList.flatMap { hoverPos =>
            Utils.bestPath(selectedPosition, hoverPos, model.units(selectedPosition).unit, model)
          }
          s.copy(hoverTile = tile, movingPath = path)
        case s: UnitAttackState => s.copy(hoverTile = tile)
        case _ => viewModel
      }
      Outcome(newVm)
    case MouseEvent.Click(Point(x, y)) =>
      val tile = Position(x / 16, y / 16)
      viewModel match {
        case OverviewState(hoverTile, endTurnButton, checkRange) if model.units.contains(tile) =>
          val movableTiles = Utils.ranges(tile, model.units(tile).unit, model)
          Outcome(UnitMoveState(hoverTile, tile, movableTiles.keys.toSet, List(tile)))
        case UnitMoveState(hoverTile, selectedPosition, movableTiles, movingPath) if movableTiles.contains(tile) =>
          val deployment = model.units(selectedPosition)
          val gunRange = Utils.inGunRange(movingPath.last, 1, deployment.unit.attackRange, model.tileMap)
          val targets = gunRange.filter(p => model.units.get(p).exists(_.player != deployment.player))
          val actions = if (targets.nonEmpty) {
            Seq("Attack" -> AttackActionEvent, "Wait" -> WaitActionEvent)
          } else {
            Seq("Wait" -> WaitActionEvent)
          }
          val actionButtons = actions.zipWithIndex.map { case ((actionStr, actionEvent), i) =>
            UIAssets.actionButton(actionStr, actionEvent)
              .moveTo(15 * 16 + 4, i * 16)
          }
          Outcome(UnitActionState(selectedPosition, movingPath, actionButtons, targets))
        case s: UnitActionState => Outcome(s)
        case UnitAttackState(selectedPosition, hoverTile, movingPath, targets) =>
          if (targets.contains(tile)) {
            Outcome(OverviewState(None, UIAssets.endTurnButton, None)).addGlobalEvents(List(MoveEvent(selectedPosition, movingPath.last), AttackEvent(movingPath.last, tile)))
          } else {
            Outcome(viewModel)
          }
        case _ => Outcome(OverviewState(None, UIAssets.endTurnButton, None))
      }
    case MouseEvent.MouseDown(Point(x, y), MouseButton.RightMouseButton) =>
      val tile = Option(Position(x / 16, y / 16)).filter(model.units.contains)
      viewModel match {
        case s: OverviewState => Outcome(s.copy(checkRange = tile))
        case _ => Outcome(viewModel)
      }
    case MouseEvent.MouseUp(_, MouseButton.RightMouseButton) =>
      viewModel match {
        case s: OverviewState => Outcome(s.copy(checkRange = None))
        case _ => Outcome(viewModel)
      }
    case GlobalEvents.AttackActionEvent =>
      viewModel match {
        case UnitActionState(selectedPosition, movingPath, actionButtons, targets) =>
          Outcome(UnitAttackState(selectedPosition, None, movingPath, targets))
        case _ => Outcome(OverviewState(None, UIAssets.endTurnButton, None))
      }
    case GlobalEvents.WaitActionEvent =>
      viewModel match {
        case UnitActionState(selectedPosition, movingPath, actionButtons, targets) =>
          Outcome(OverviewState(None, UIAssets.endTurnButton, None)).addGlobalEvents(MoveEvent(selectedPosition, movingPath.last))
        case _ => Outcome(OverviewState(None, UIAssets.endTurnButton, None))
      }
    case FrameTick =>
      viewModel match {
        case s: OverviewState =>
          s.endTurnButton.update(context.inputState.mouse).map { btn =>
            s.copy(endTurnButton = btn)
          }
        case s: UnitActionState =>
          s.actionButtons
            .map(b => b.update(context.inputState.mouse))
            .sequence
            .map(btns => s.copy(actionButtons = btns))
        case _ => Outcome(viewModel)
      }

    case _ => Outcome(viewModel)
  }

  // TODO: Move some of this logic out of the present loop and into the updateViewModel
  override def present(context: FrameContext[Unit], model: GameState, viewModel: UIState): Outcome[SceneUpdateFragment] = {
    val tiles = model.tileMap.map.map { case (Position(x, y), i) =>
      val tilePos = tileAtlas.getOrElse(i, (0, 6))
      Graphic(Rectangle(tilePos._1 * 16, tilePos._2 * 16, 16, 16), 3, Material.Bitmap(tileSetAssetName)).moveTo(Point(x * 16, y * 16))
    }.toList

    val units = model.units.map { case (Position(x, y), u) =>
      Graphic(Rectangle(3 * 16, 0, 16, 16), 3, Material.Bitmap(tileSetAssetName)).moveTo(Point(x * 16, y * 16))
    }.toList

    def drawToMoveUnit(position: Position) =
      List(Graphic(Rectangle(3 * 16, 0, 16, 16), 3, Material.Bitmap(tileSetAssetName)).moveTo(Point(position.x * 16, position.y * 16)))


    def hoverCursor(hoverTile: Option[Position]) = hoverTile.flatMap { case p@Position(x, y) =>
      if (model.tileMap.map.contains(p))
        Some(Graphic(Rectangle(3 * 16, 2 * 16, 16, 16), 2, Material.Bitmap(tileSetAssetName)).moveTo(Point(x * 16, y * 16)))
      else
        None
    }.toList


    def sideMenuText(text: String, i: Int) =
      TextBox(text, 200, 16)
        .withFontFamily(FontFamily.monospace)
        .withColor(RGBA.White)
        .withFontSize(Pixels(12))
        .alignStart
        .moveTo(15 * 16 + 4, i * 16)

    def tileInfo(hoverTile: Option[Position]) = hoverTile.flatMap(model.tileMap.tileAt)
      .toList
      .flatMap { tile =>
        List(
          sideMenuText(tile.name, 0),
          sideMenuText(s"Def: ${tile.defense}", 1))
      }

    def unitInfo(hoverTile: Option[Position]) = hoverTile.flatMap(model.units.get).toList.flatMap { deployment =>
      List(
        sideMenuText(deployment.unit.name, 2),
        sideMenuText(s"${deployment.health}/100", 3),
        sideMenuText(s"P: ${deployment.player}", 4))
    }

    def selectedUnitRange(tiles: Set[Position]) =
      tiles.map { case Position(x, y) =>
        Shape.Box(
          Rectangle(Point(x * 16, y * 16), Size(16, 16)),
          Fill.Color(RGBA.White.withAlpha(0.4)),
          Stroke(1, RGBA.SteelBlue)
        ).withDepth(Depth(2))
      }

    def pathLines(path: Seq[Position]) =
      if (path.size > 1) {
        val pointPath = path.map { case Position(x, y) => Point(x * 16 + 8, y * 16 + 8) }
        pointPath.zip(pointPath.tail).map { case (from, to) =>
          Shape.Line(from, to, Stroke(4, RGBA.Red))
        }
      } else {
        List()
      }

    def rangeCheck(checkInfo: Option[Position]) = checkInfo.flatMap(p => model.units.get(p).map(p -> _)).toList.flatMap { case (position, deployment) =>
      val movableTiles = Utils.ranges(position, deployment.unit, model)
      val tiles = movableTiles.flatMap { mt =>
        Utils.inGunRange(mt._1, 1, deployment.unit.attackRange, model.tileMap)
      }.toSet

      tiles.map { case Position(x, y) =>
        Shape.Box(
          Rectangle(Point(x * 16, y * 16), Size(16, 16)),
          Fill.Color(RGBA.Red.withAlpha(0.6)),
          Stroke(1, RGBA.Red)
        ).withDepth(Depth(2))
      }
    }

    def targetsSquares(targets: Set[Position]) = targets.map { case Position(x, y) =>
      Shape.Box(
        Rectangle(Point(x * 16, y * 16), Size(16, 16)),
        Fill.Color(RGBA.Red.withAlpha(0.6)),
        Stroke(1, RGBA.Red)
      ).withDepth(Depth(2))
    }


    viewModel match {
      case OverviewState(hoverTile, endTurnButton, checkRange) =>
        val allElements = tiles ++ units ++ hoverCursor(hoverTile) ++ tileInfo(hoverTile) ++ unitInfo(hoverTile) ++ rangeCheck(checkRange) ++ List(endTurnButton.draw)
        Outcome(SceneUpdateFragment(allElements))
      case UnitMoveState(hoverTile, selectedPosition, movableTiles, movingPath) =>
        val allElements = tiles ++ units ++ hoverCursor(hoverTile) ++ tileInfo(hoverTile) ++ unitInfo(hoverTile) ++ selectedUnitRange(movableTiles) ++ pathLines(movingPath)
        Outcome(SceneUpdateFragment(allElements))
      case UnitActionState(selectedUnit, movingPath, actionButtons, targets) =>
        val allElements = tiles ++ units ++ pathLines(movingPath) ++ actionButtons.map(_.draw) ++ drawToMoveUnit(movingPath.last)
        Outcome(SceneUpdateFragment(allElements))
      case UnitAttackState(selectedUnit, hoverTile, movingPath, targets) =>
        val allElements = tiles ++ units ++ hoverCursor(hoverTile) ++ tileInfo(hoverTile) ++ unitInfo(hoverTile) ++ targetsSquares(targets) ++ pathLines(movingPath) ++ drawToMoveUnit(movingPath.last)
        Outcome(SceneUpdateFragment(allElements))
    }
  }
}
