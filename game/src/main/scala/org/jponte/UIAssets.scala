package org.jponte

import indigo.*
import indigoextras.ui.{Button, ButtonAssets}
import GlobalEvents.EndTurnEvent

object UIAssets {

  private val buttonUpAssetName = AssetName("button_up")
  private val buttonDownAssetName = AssetName("button_down")

  private def buttonAssets(text: String): ButtonAssets = ButtonAssets(
    up = TextBox(text, 200, 16)
      .withFontFamily(FontFamily.fantasy)
      .withColor(RGBA.Teal)
      .withFontSize(Pixels(12))
      .alignStart,
    over = TextBox(text, 200, 16)
      .withFontFamily(FontFamily.fantasy)
      .withColor(RGBA.SteelBlue)
      .withFontSize(Pixels(12))
      .alignStart,
    down = TextBox(text, 200, 16)
      .withFontFamily(FontFamily.fantasy)
      .withColor(RGBA.DarkBlue)
      .withFontSize(Pixels(12))
      .alignStart
  )

  def endTurnButton: Button = Button(
    buttonAssets = buttonAssets("End Turn"),
    bounds = Rectangle(0, 0, 32, 16),
    depth = Depth(1)
  ).withUpActions(EndTurnEvent).moveTo(15 * 16 + 4, 132)

  def actionButton(action: String, event: GlobalEvent): Button = Button(
    buttonAssets = buttonAssets(action),
    bounds = Rectangle(0, 0, 32, 16),
    depth = Depth(1)
  ).withUpActions(event)
}
