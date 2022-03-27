package org.jponte

import scala.annotation.tailrec

object Utils {
  def ranges(tileMap: TileMap, from: Position): Map[Position, Int] = {

    @tailrec
    def scanPositions(positions: Seq[Position], costs: Map[Position, Int]): Map[Position, Int] = {

      positions.headOption match {
        case Some(pos@Position(x, y)) =>
          val currentCost = costs(pos)
          val newCost = currentCost + 1
          val nextCosts = Seq(Position(x + 1, y), Position(x - 1, y), Position(x, y + 1), Position(x, y - 1))
            .collect {
              case pos if tileMap.map.keys.exists(_ == pos) && costs.get(pos).forall(_ > newCost) =>
                pos -> costs.get(pos).fold(newCost)(oldCost => Math.min(newCost, oldCost))
            }.toMap
          scanPositions(positions.tail ++ nextCosts.keys.toSeq, costs ++ nextCosts)
        case None => costs
      }
    }

    scanPositions(Seq(from), Map(from -> 0))
  }
}
