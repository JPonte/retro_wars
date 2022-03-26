package org.jponte

import scala.util.Random

object Main {
  def main(args: Array[String]): Unit = {
    val gameState = getRandomState(8, 5, 1)
    println(gameState.show)

    val newState = gameState.runAction(Attack(Position(0, 1), Position(0, 2)))
    println(newState.map(_.units))
  }

  def getRandomState(width: Int, height: Int, seed: Long): GameState = {
    Random.setSeed(seed)
    val tiles = (for {
      x <- 0 until width
      y <- 0 until height
    } yield Position(x, y) -> Random.nextInt(Tile.allTiles.size)).toMap
    GameState(TileMap(width, height, Tile.allTiles, tiles), Map(Position(0, 1) -> Deployment(Character.Infantry, 0), Position(0, 2) -> Deployment(Character.Infantry, 1)), Seq(Player("P0"), Player("P1")), 0)
  }
}
