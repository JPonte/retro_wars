package org.jponte

sealed case class Character(name: String, symbol: Char, baseAttack: Int, attackRange: Int, moveRange: Int)

object Character {
  val Infantry: Character = Character("Infantry", 'I', 10, 1, 3)
  val Tank: Character = Character("Tank", 'T', 50, 1, 5)

  val allCharacters = Seq(Infantry, Tank)
  val infantryCharacters: Set[Character] = Set(Infantry)
}
