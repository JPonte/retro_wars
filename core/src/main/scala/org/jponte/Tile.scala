package org.jponte

case class Tile(name: String, symbol: String, defense: Int)

object Tile {
  val Grass: Tile = Tile("Grass", "#", 1)
  val Road: Tile = Tile("Road", "=", 0)
  val Forest: Tile = Tile("Forest", "*", 4)
  val Sea: Tile = Tile("Sea", "~", 1)

  val City: Tile = Tile("City", "X", 3)
  val Factory: Tile = Tile("Factory", "F", 3)
  val Airport: Tile = Tile("Airport", "A", 3)
  val Port: Tile = Tile("Port", "P", 3)

  val allTiles: Seq[Tile] = Seq(Grass, Road, Forest, Sea, City, Factory, Airport, Port)
  val cities: Set[Tile] = Set(City, Factory, Airport, Port)
}