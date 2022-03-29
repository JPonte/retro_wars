package org.jponte

import scala.annotation.tailrec
import scala.util.Random

object Utils {
  def ranges(from: Position, character: Character, gameState: GameState): Map[Position, Int] = {

    @tailrec
    def scanPositions(positions: Seq[Position], costs: Map[Position, Int]): Map[Position, Int] = {
      positions.headOption match {
        case Some(pos) =>
          val currentCost = costs(pos)
          val adjacentTiles = pos.adjacentPositions
          val nextCosts = adjacentTiles.flatMap { pos =>
            gameState.tileMap.tileAt(pos).map(pos -> _)
          }.flatMap { case (pos, tile) =>
            for {
              tileCost <- tile.moveCost.get(character.movementType)
              newCost = tileCost + currentCost
              if pos == from || !gameState.units.contains(pos)
              if newCost <= character.moveRange
              if !costs.get(pos).exists(_ <= newCost)
            } yield pos -> newCost
          }.toMap
          scanPositions(positions.tail ++ nextCosts.keys.toSeq, costs ++ nextCosts)
        case None => costs
      }
    }

    scanPositions(Seq(from), Map(from -> 0))
  }

  def inGunRange(from: Position, minRange: Int, maxRange: Int, tileMap: TileMap): Set[Position] = {
    def dist(p1: Position, p2: Position): Int = (p2.x - p1.x).abs + (p2.y - p1.y).abs

    tileMap.map.keys.filter { to =>
      val d = dist(from, to)
      d >= minRange && d <= maxRange
    }.toSet
  }

  def bestPath(from: Position, to: Position, character: Character, gameState: GameState): List[Position] = {
    val costs = ranges(from, character, gameState)

    @tailrec
    def rec(currentPos: Position, acc: List[Position]): List[Position] = {
      if (acc.contains(from)) {
        acc
      } else {
        val adjacent = currentPos.adjacentPositions.filter(!acc.contains(_)).flatMap(p => costs.get(p).map(p -> _)).toMap
        if (adjacent.isEmpty) {
          acc
        } else {
          val next = adjacent.minBy(_._2)
          rec(next._1, acc :+ next._1)
        }
      }
    }

    if (costs.contains(to))
      rec(to, List(to)).reverse
    else
      List()
  }

  def getRandomState(width: Int, height: Int, seed: Long): GameState = {
    Random.setSeed(seed)
    val tiles = (for {
      x <- 0 until width
      y <- 0 until height
    } yield Position(x, y) -> Random.nextInt(Tile.allTiles.size - 3)).toMap
    GameState(
      TileMap(width, height, Tile.allTiles, tiles),
      Map(Position(4, 4) -> Deployment(Character.Tank, 0), Position(1, 2) -> Deployment(Character.Infantry, 1)),
      Map(),
      Seq(Player("P0"), Player("P1")),
      0)
  }

  def testMap: GameState = {
    val tiles = testMapStr.split("\n").zipWithIndex.flatMap { case (line, y) =>
      line.trim.split(",").zipWithIndex.map { case (char, x) =>
        (Position(x, y), char.toInt)
      }
    }.toMap
    GameState(
      TileMap(15, 10, Tile.allTiles, tiles),
      Map(Position(4, 4) -> Deployment(Character.Tank, 0), Position(1, 2) -> Deployment(Character.Infantry, 1)),
      Map(),
      Seq(Player("P0"), Player("P1")),
      0)
  }

  val testMapStr: String =
    """3,3,3,3,3,3,3,3,3,3,3,3,3,3,3
      |3,3,0,2,1,1,1,1,1,0,2,4,0,4,3
      |3,0,0,2,1,2,3,0,1,4,0,0,4,4,3
      |3,4,4,1,1,3,3,3,1,0,2,0,1,4,3
      |3,0,0,1,0,3,3,3,1,4,2,4,1,0,3
      |3,4,1,1,4,3,3,2,1,1,1,1,1,4,3
      |3,4,4,0,0,0,0,2,2,0,3,3,1,3,3
      |3,3,4,4,0,0,0,0,0,0,3,3,0,0,3
      |3,3,3,3,2,4,2,2,4,0,1,0,4,4,3
      |3,3,3,3,3,3,3,3,3,3,3,3,3,3,3""".stripMargin
}
