package org.jponte

import indigo.*
import indigoextras.ui.{Button, ButtonAssets}
import GlobalEvents.{CancelActionEvent, EndTurnEvent, PurchaseUnitEvent}
import indigo.shared.input.Mouse
import org.jponte.UIAssets.actionButton
import indigo.Outcome.sequence

enum PopUpDirection {
  case NW, NE, SW, SE
}
case class ActionBox(buttons: List[Button], pivot: Point, direction: PopUpDirection) {

  private def ref: (Int, Int) = {
    val boxWidth = 64
    val boxHeight = 16 * buttons.size + 4

    direction match {
      case PopUpDirection.NW => (boxWidth, boxHeight)
      case PopUpDirection.NE => (0, boxHeight)
      case PopUpDirection.SW => (boxWidth, 0)
      case PopUpDirection.SE => (0, 0)
    }
  }

  private def buttonPosition(index: Int): Point = {
    val (xRef, yRef) = ref

    Point(pivot.x - xRef + 4, pivot.y + index * 16 + 4 - yRef)
  }

  def updateButtons(mouseState: Mouse): Outcome[ActionBox] =
    buttons
      .zipWithIndex
      .map { case (btn, i) => btn.moveTo(buttonPosition(i)) }
      .map(_.update(mouseState))
      .sequence
      .map(btns => copy(buttons = btns))

  def draw: List[SceneNode] = {
    val boxWidth = 64
    val boxHeight = 16 * buttons.size + 4

    val (xRef, yRef) = ref

    List(
      Shape
        .Box(
          Rectangle(boxWidth, boxHeight),
          Fill.Color(RGBA.Black),
          Stroke(2, RGBA.SlateGray)
        )
        .withRef(xRef, yRef)
        .moveTo(pivot.x, pivot.y)
        .withDepth(Depth(2))
    ) ++ buttons.zipWithIndex.map {
      case (button, index) =>
        button.moveTo(buttonPosition(index)).draw
    }
  }
}

object ActionBox {
  def apply(
      actions: List[(String, GlobalEvent)],
      position: Position,
      tileMap: TileMap
  ): ActionBox = {
    val (pivot, direction) = bestDirection(position, tileMap)
    ActionBox(
      actions.map { case (str, event) => actionButton(str, event) },
      pivot,
      direction
    )
  }

  def bestDirection(position: Position, tileMap: TileMap): (Point, PopUpDirection) =
    (position.x > tileMap.width / 2, position.y > tileMap.height / 2) match {
      case (true, true) => (Point(position.x * 16, position.y * 16), PopUpDirection.NW)
      case (true, false) => (Point(position.x * 16, (position.y + 1) * 16), PopUpDirection.SW)
      case (false, true) => (Point((position.x + 1) * 16, position.y * 16), PopUpDirection.NE)
      case (false, false) =>
        (Point((position.x + 1) * 16, (position.y + 1) * 16), PopUpDirection.SE)
    }
}

case class PurchaseMenu(buttons: List[Button], skipFrame: Boolean = true) {
  def updateButtons(mouseState: Mouse): Outcome[PurchaseMenu] =
    if (skipFrame) {
      Outcome(this.copy(skipFrame = false))
    } else {
      buttons.map(_.update(mouseState)).sequence.map(btns => copy(buttons = btns))
    }

  def draw: List[SceneNode] = List(
    Shape
      .Box(
        Rectangle(13 * 16, 8 * 16),
        Fill.Color(RGBA.Black),
        Stroke(2, RGBA.SlateGray)
      )
      .moveTo(32, 32)
      .withDepth(Depth(2))
  ) ++ buttons.map(_.draw)
}

object PurchaseMenu {
  def apply(units: List[Character], funds: Long, position: Position): PurchaseMenu = {
    val actions = units.map { c =>
      s"${c.name} - $$${c.cost}" -> PurchaseUnitEvent(position, c)
    } :+ ("Cancel" -> CancelActionEvent)

    val buttons = actions.zipWithIndex.map {
      case ((actionStr, actionEvent), i) =>
        UIAssets.actionButton(actionStr, actionEvent).moveTo(36, 36 + i * 16)
    }
    PurchaseMenu(buttons)
  }
}

object UIAssets {

  private val buttonUpAssetName = AssetName("button_up")
  private val buttonDownAssetName = AssetName("button_down")

  private def buttonAssets(text: String, fontSize: Int): ButtonAssets = ButtonAssets(
    up = TextBox(text, text.length * 8, fontSize + 2)
      .withFontFamily(FontFamily.sansSerif)
      .withColor(RGBA.White)
      .withStroke(TextStroke(RGBA.Black, Pixels(2)))
      .withFontSize(Pixels(fontSize))
      .alignStart,
    over = TextBox(text, text.length * 8, fontSize + 2)
      .withFontFamily(FontFamily.sansSerif)
      .withColor(RGBA.SlateGray)
      .withStroke(TextStroke(RGBA.Black, Pixels(2)))
      .withFontSize(Pixels(fontSize))
      .alignStart,
    down = TextBox(text, text.length * 8, fontSize + 2)
      .withFontFamily(FontFamily.sansSerif)
      .withColor(RGBA.Black)
      .withStroke(TextStroke(RGBA.Black, Pixels(2)))
      .withFontSize(Pixels(fontSize))
      .alignStart
  )

  def endTurnButton: Button = Button(
    buttonAssets = buttonAssets("End Turn", 12),
    bounds = Rectangle(0, 0, 48, 16),
    depth = Depth(1)
  ).withUpActions(EndTurnEvent).moveTo(13 * 16, 10 * 16 + 8)

  def actionButton(action: String, event: GlobalEvent): Button =
    Button(
      buttonAssets = buttonAssets(action, 10),
      bounds = Rectangle(0, 0, action.length * 6, 16),
      depth = Depth(1)
    ).withUpActions(event)
  private val colorSequence = Seq(RGB.SteelBlue, RGB.Red, RGB.Yellow, RGB.Green)

  def playerInfoBox(player: Player, playerIndex: Int, switchSide: Boolean): List[SceneNode] = {
    val boxX = if (switchSide) 17 * 16 + 2 - 64 else 2
    val textX = if (switchSide) 17 * 16 + 5 - 64 else 5
    List(
      Shape
        .Box(
          Rectangle(64, 32),
          Fill.Color(colorSequence(playerIndex).toRGBA.mix(RGBA.Black)),
          Stroke(2, RGBA.SlateGray)
        )
        .moveTo(boxX, 2)
        .withDepth(Depth(2)),
      TextBox(player.name, 64, 16)
        .withFontFamily(FontFamily.sansSerif)
        .withColor(RGBA.White)
        .withFontSize(Pixels(10))
        .alignStart
        .moveTo(textX, 4),
      TextBox(s"$$${player.funds}", 64, 16)
        .withFontFamily(FontFamily.sansSerif)
        .withColor(RGBA.White)
        .withFontSize(Pixels(10))
        .alignStart
        .moveTo(textX, 18)
    )
  }

  private def tileInfoText(text: String, i: Int) =
    TextBox(text, 64, 10)
      .withFontFamily(FontFamily.sansSerif)
      .withColor(RGBA.White)
      .withFontSize(Pixels(8))
      .alignStart
      .moveTo(4, 8 * 16 + i * 10 + 10)

  def tileInfoBox(
      tile: Option[Tile],
      deployment: Option[Deployment],
      cityStatus: Option[CityStatus]
  ): List[SceneNode] = {
    List(
      Shape
        .Box(
          Rectangle(54, 54),
          Fill.Color(RGBA.Black.withAlpha(0.5))
        )
        .withRef(0, 54)
        .moveTo(2, 12 * 16 - 2)
        .withDepth(Depth(2))
    ) ++ tile.toList.flatMap { t =>
      List(
        tileInfoText(t.name, 0),
        tileInfoText((0 until t.defense).map(_ => "\uD83D\uDEE1").mkString(""), 1)
      )
    } ++ deployment.toList.flatMap { d =>
      List(tileInfoText(d.unit.name, 2), tileInfoText(s"${d.health}/100", 3))
    } ++ cityStatus.flatMap(_.underSiege).toList.flatMap { siege =>
      List(tileInfoText(s"${siege.remaining}/20 🏢", 4))
    }
  }

}
