package org.jponte

case class Tile(name: String, symbol: String, defense: Int)

object Tile {
  val Grass: Tile = Tile("Grass", "#", 1)
  val Road: Tile = Tile("Road", "=", 0)
  val City: Tile = Tile("City", "X", 3)

  val allTiles: Seq[Tile] = Seq(Grass, Road, City)
}