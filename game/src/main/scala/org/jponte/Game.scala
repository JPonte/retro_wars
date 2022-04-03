package org.jponte

import cats.{Align, Alternative, Applicative, CoflatMap, Eval, Foldable, Functor, Monad, Traverse}
import indigo.Outcome.sequence
import indigo.*
import indigo.shared.events.MouseButton
import indigoextras.subsystems.FPSCounter
import indigoextras.ui.{Button, ButtonAssets}
import org.jponte.GlobalEvents.*
import io.circe.syntax.*
import io.circe.parser.decode

import scala.scalajs.js.annotation.JSExportTopLevel

@JSExportTopLevel("IndigoGame")
object Game extends IndigoDemo[Unit, WebSocketConfig, GameState, UIState] {

  override def eventFilters: EventFilters = EventFilters.Permissive

  private val magnification = 3
  private val colorSequence = Seq(RGB.SteelBlue, RGB.Red, RGB.Yellow, RGB.Green)
  private val webSocketId = WebSocketId("game_sock")

  override def boot(flags: Map[String, String]): Outcome[BootResult[Unit]] =
    Outcome(
      BootResult
        .noData(
          GameConfig
            .default
            .withViewport(17 * 16 * magnification, 12 * 16 * magnification)
            .withMagnification(magnification)
        )
        .withAssets(TileAssets.tileSet)
        .withAnimations(TileAssets.seaAnimation)
    )

  override def setup(
      bootData: Unit,
      assetCollection: AssetCollection,
      dice: Dice
  ): Outcome[Startup[WebSocketConfig]] =
    Outcome(
      Startup.Success(
        WebSocketConfig(
          id = webSocketId,
          address = "ws://localhost:8080/ws"
        )
      )
    )

  override def initialViewModel(startupData: WebSocketConfig, model: GameState): Outcome[UIState] =
    Outcome(OverviewState(None, UIAssets.endTurnButton, None))

  override def initialModel(startupData: WebSocketConfig): Outcome[GameState] =
    Outcome(Utils.testMap).addGlobalEvents(WebSocketEvent.Open("hello", startupData))

  private def runAction(action: GameAction, model: GameState): Outcome[GameState] = {
    model.runAction(action) match {
      case Left(error) =>
        IndigoLogger.error(error)
        Outcome(model)
      case Right(newModel) => Outcome(newModel)
    }
  }

  private def runActionAndSend(
      action: GameAction,
      context: FrameContext[WebSocketConfig],
      model: GameState
  ): Outcome[GameState] = {
    model.runAction(action) match {
      case Left(error) =>
        IndigoLogger.error(error)
        Outcome(model)
      case Right(newModel) =>
        Outcome(newModel).addGlobalEvents(
          WebSocketEvent.Send(
            GameActionWebSocketMessage(action).asInstanceOf[WebSocketMessage].asJson.noSpaces,
            context.startUpData
          )
        )
    }
  }

  override def updateModel(
      context: FrameContext[WebSocketConfig],
      model: GameState
  ): GlobalEvent => Outcome[GameState] = {
    case EndTurnEvent =>
      runActionAndSend(EndTurn, context, model)
    case MoveEvent(from, to) =>
      runActionAndSend(Move(from, to), context, model)
    case AttackEvent(from, to) =>
      runActionAndSend(Attack(from, to), context, model)
    case PurchaseUnitEvent(position, character) =>
      runActionAndSend(PurchaseUnit(position, character), context, model)
    case CaptureCityEvent(position) =>
      runActionAndSend(CaptureCity(position), context, model)
    case WebSocketEvent.Receive(webSocketId, message) =>
      decode[WebSocketMessage](message) match {
        case Left(value) =>
          IndigoLogger.error(s"Couldn't decode $message - Error: $value")
          Outcome(model)
        case Right(GameActionWebSocketMessage(gameAction)) => runAction(gameAction, model)
        case Right(GameStateWebSocketMessage(gameState)) => Outcome(gameState)
        case other =>
          IndigoLogger.error(s"Got something weird: $other")
          Outcome(model)
      }
    case WebSocketEvent.Error(WebSocketId(id), message) =>
      val msg = s"Connection [$id] errored with: " + message
      IndigoLogger.consoleLog(msg)
      Outcome(model)

    case WebSocketEvent.Close(WebSocketId(id)) =>
      val msg = s"Connection [$id] closed."
      IndigoLogger.consoleLog(msg)
      Outcome(model)

    case _ => Outcome(model)
  }

  override def updateViewModel(
      context: FrameContext[WebSocketConfig],
      model: GameState,
      viewModel: UIState
  ): GlobalEvent => Outcome[UIState] = {
    case MouseEvent.Move(Point(x, y)) =>
      val tile =
        Option(Position(x / 16, y / 16)).filter(model.tileMap.map.contains)
      val newVm = viewModel match {
        case s: OverviewState => s.copy(hoverTile = tile)
        case s @ UnitMoveState(_, selectedPosition, _, _) =>
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
        case OverviewState(hoverTile, endTurnButton, checkRange)
            if model.units.get(tile).exists(d => d.canMove && d.player == model.currentPlayer) =>
          val movableTiles = Utils.ranges(tile, model.units(tile).unit, model)
          Outcome(UnitMoveState(hoverTile, tile, movableTiles.keys.toSet, List(tile)))
        case OverviewState(hoverTile, endTurnButton, checkRange)
            if !model
              .units
              .contains(tile) && model.tileMap.tileAt(tile).contains(Tile.Factory) && model
              .cities
              .get(tile)
              .exists(_.owner.contains(model.currentPlayer)) =>
          val units = Character.allCharacters.filter(_.cost <= model.getCurrentPlayer.funds).toList
          Outcome(
            PurchaseUnitActionState(tile, PurchaseMenu(units, model.getCurrentPlayer.funds, tile))
          )
        case UnitMoveState(hoverTile, selectedPosition, movableTiles, movingPath)
            if movableTiles.contains(tile) =>
          val deployment = model.units(selectedPosition)
          val gunRange =
            Utils.inGunRange(
              movingPath.last,
              deployment.unit.minAttackRange,
              deployment.unit.maxAttackRange,
              model.tileMap
            )
          val targets =
            gunRange.filter(p => model.units.get(p).exists(_.player != deployment.player))
          val canAttack = deployment.unit.hasActionAfterMove || movingPath.size == 1
          val isCity = model.tileMap.tileAt(tile).exists(t => Tile.cities.contains(t))
          val cityStatus = model.cities.get(tile).forall(!_.owner.contains(model.currentPlayer))
          val captureAction =
            if (isCity && cityStatus) List("Capture" -> CaptureCityActionEvent) else List()
          val attackAction =
            if (targets.nonEmpty && canAttack) List("Attack" -> AttackActionEvent) else List()
          val actions = attackAction ++ captureAction ++ List(
            "Wait" -> WaitActionEvent,
            "Cancel" -> CancelActionEvent
          )

          Outcome(
            UnitActionState(
              selectedPosition,
              movingPath,
              ActionBox(actions, movingPath.last, model.tileMap),
              targets
            )
          )
        case s: UnitActionState => Outcome(s)
        case UnitAttackState(selectedPosition, hoverTile, movingPath, targets)
            if targets.contains(tile) =>
          Outcome(OverviewState(None, UIAssets.endTurnButton, None)).addGlobalEvents(
            MoveEvent(selectedPosition, movingPath.last),
            AttackEvent(movingPath.last, tile)
          )
        case _ => Outcome(viewModel)
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
        case _ => Outcome(OverviewState(None, UIAssets.endTurnButton, None))
      }
    case GlobalEvents.AttackActionEvent =>
      viewModel match {
        case UnitActionState(selectedPosition, movingPath, actionButtons, targets) =>
          Outcome(UnitAttackState(selectedPosition, None, movingPath, targets))
        case _ => Outcome(OverviewState(None, UIAssets.endTurnButton, None))
      }
    case GlobalEvents.CaptureCityActionEvent =>
      viewModel match {
        case UnitActionState(selectedPosition, movingPath, actionButtons, targets) =>
          Outcome(OverviewState(None, UIAssets.endTurnButton, None)).addGlobalEvents(
            MoveEvent(selectedPosition, movingPath.last),
            CaptureCityEvent(movingPath.last)
          )
        case _ => Outcome(OverviewState(None, UIAssets.endTurnButton, None))
      }
    case GlobalEvents.WaitActionEvent =>
      viewModel match {
        case UnitActionState(selectedPosition, movingPath, actionButtons, targets) =>
          Outcome(OverviewState(None, UIAssets.endTurnButton, None))
            .addGlobalEvents(MoveEvent(selectedPosition, movingPath.last))
        case _ => Outcome(OverviewState(None, UIAssets.endTurnButton, None))
      }
    case GlobalEvents.CancelActionEvent =>
      viewModel match {
        case s: OverviewState => Outcome(s)
        case _ => Outcome(OverviewState(None, UIAssets.endTurnButton, None))
      }
    case PurchaseUnitEvent(_, _) =>
      Outcome(OverviewState(None, UIAssets.endTurnButton, None))
    case FrameTick =>
      viewModel match {
        case s: OverviewState =>
          s.endTurnButton.update(context.inputState.mouse).map { btn =>
            s.copy(endTurnButton = btn)
          }
        case s: UnitActionState =>
          s.actionButtons
            .updateButtons(context.inputState.mouse)
            .map(a => s.copy(actionButtons = a))
        case s: PurchaseUnitActionState =>
          s.purchaseMenu.updateButtons(context.inputState.mouse).map(a => s.copy(purchaseMenu = a))
        case _ => Outcome(viewModel)
      }
    case _ => Outcome(viewModel)
  }

  override def present(
      context: FrameContext[WebSocketConfig],
      model: GameState,
      viewModel: UIState
  ): Outcome[SceneUpdateFragment] = {
    val units = model.units.toList.flatMap {
      case (position, deployment) =>
        TileAssets.drawDeployment(position, deployment)
    }

    def hoverCursor(hoverTile: Option[Position]) = hoverTile.flatMap {
      case p @ Position(x, y) =>
        if (model.tileMap.map.contains(p))
          Some(
            Graphic(
              Rectangle(6 * 16, 42 * 16, 16, 16),
              0,
              Material.Bitmap(TileAssets.tileSetAssetName)
            ).moveTo(Point(x * 16, y * 16))
          )
        else
          None
    }.toList

    def selectedUnitRange(tiles: Set[Position]) =
      tiles.map {
        case Position(x, y) =>
          Shape
            .Box(
              Rectangle(Point(x * 16, y * 16), Size(16, 16)),
              Fill.Color(RGBA.White.withAlpha(0.4)),
              Stroke(1, RGBA.SteelBlue)
            )
            .withDepth(Depth(2))
      }

    def pathLines(path: Seq[Position]) =
      if (path.size > 1) {
        val pointPath = path.map {
          case Position(x, y) =>
            Point(x * 16 + 6, y * 16 + 6)
        }
        pointPath.zip(pointPath.tail).map {
          case (from, to) =>
            Shape.Line(from, to, Stroke(4, RGBA.Red)).withDepth(Depth(3))
        }
      } else {
        List()
      }

    def rangeCheck(checkInfo: Option[Position]) =
      checkInfo.flatMap(p => model.units.get(p).map(p -> _)).toList.flatMap {
        case (position, deployment) =>
          val movableTiles =
            if (deployment.unit.hasActionAfterMove)
              Utils.ranges(position, deployment.unit, model).keys
            else Seq(position)
          val tiles = movableTiles.flatMap { mt =>
            Utils.inGunRange(
              mt,
              deployment.unit.minAttackRange,
              deployment.unit.maxAttackRange,
              model.tileMap
            )
          }.toSet

          tiles.map {
            case Position(x, y) =>
              Shape
                .Box(
                  Rectangle(Point(x * 16, y * 16), Size(16, 16)),
                  Fill.Color(RGBA.Red.withAlpha(0.6)),
                  Stroke(1, RGBA.Red)
                )
                .withDepth(Depth(2))
          }
      }

    def targetsSquares(targets: Set[Position]) = targets.map {
      case Position(x, y) =>
        Shape
          .Box(
            Rectangle(Point(x * 16, y * 16), Size(16, 16)),
            Fill.Color(RGBA.Red.withAlpha(0.6)),
            Stroke(1, RGBA.Red)
          )
          .withDepth(Depth(2))
    }

    viewModel match {
      case OverviewState(hoverTile, endTurnButton, checkRange) =>
        val allElements = TileAssets.drawMap(model) ++ units ++ hoverCursor(
          hoverTile
        ) ++ rangeCheck(checkRange) ++ List(endTurnButton.draw) ++ UIAssets.playerInfoBox(
          model.players(model.currentPlayer),
          model.currentPlayer,
          hoverTile.exists(p => p.x < model.tileMap.width / 4 && p.y < model.tileMap.height / 4)
        ) ++ UIAssets.tileInfoBox(
          hoverTile.flatMap(model.tileMap.tileAt),
          hoverTile.flatMap(model.units.get),
          hoverTile.flatMap(model.cities.get)
        )
        Outcome(SceneUpdateFragment(allElements))
      case UnitMoveState(hoverTile, selectedPosition, movableTiles, movingPath) =>
        val allElements =
          TileAssets.drawMap(model) ++ pathLines(movingPath) ++ units ++ hoverCursor(
            hoverTile
          ) ++ selectedUnitRange(
            movableTiles
          )
        Outcome(SceneUpdateFragment(allElements))
      case UnitActionState(selectedUnit, movingPath, actionButtons, targets) =>
        val allElements =
          TileAssets.drawMap(model) ++ pathLines(
            movingPath
          ) ++ units ++ actionButtons.draw ++ TileAssets.drawDeployment(
            movingPath.last,
            model.units(selectedUnit),
            true
          )
        Outcome(SceneUpdateFragment(allElements))
      case UnitAttackState(selectedUnit, hoverTile, movingPath, targets) =>
        val allElements =
          TileAssets.drawMap(model) ++ pathLines(movingPath) ++ units ++ hoverCursor(
            hoverTile
          ) ++ targetsSquares(targets) ++
            TileAssets.drawDeployment(movingPath.last, model.units(selectedUnit), true)
        Outcome(SceneUpdateFragment(allElements))
      case PurchaseUnitActionState(selectedPosition, purchaseMenu) =>
        val allElements = TileAssets.drawMap(model) ++ units ++ purchaseMenu.draw
        Outcome(SceneUpdateFragment(allElements))
    }
  }
}
