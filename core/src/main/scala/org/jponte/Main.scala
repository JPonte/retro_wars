package org.jponte

object Main {
  def main(args: Array[String]): Unit = {
    val gameState = Utils.testMap
    println(gameState.show)

    val newState = gameState.runAction(Attack(Position(0, 1), Position(0, 2)))
    println(newState.map(_.units))
  }
}
