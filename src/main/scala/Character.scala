package org.jponte

sealed case class Character(symbol: Char, baseAttack: Int, range: Int)

object Character {
  val Infantry: Character = Character('I', 10, 1)
  val Tank: Character = Character('T', 50, 1)
}
