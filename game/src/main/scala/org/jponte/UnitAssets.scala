package org.jponte

import indigo._

trait ToRGBA[A] {
  def withAlpha(a: A, alpha: Double): RGBA

  def toRGBA(a: A): RGBA
}

class RGBtoRGBA extends ToRGBA[RGB] {
  override def withAlpha(a: RGB, alpha: Double): RGBA =
    RGBA(a.r, a.g, a.b, alpha)

  override def toRGBA(a: RGB): RGBA = RGBA(a.r, a.g, a.b, 1.0)
}

object UnitAssets {
  val colorSequence = Seq(RGB.Green, RGB.Red, RGB.Yellow, RGB.Blue)
}
