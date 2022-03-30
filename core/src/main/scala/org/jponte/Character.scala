package org.jponte

sealed case class Character(
    name: String,
    symbol: Char,
    baseAttack: Int,
    attackRange: Int,
    moveRange: Int,
    movementType: MovementType
)

object Character {
  val Infantry: Character =
    Character("Infantry", 'I', 10, 1, 3, MovementType.Foot)
  val Tank: Character = Character("Tank", 'T', 50, 1, 5, MovementType.Tread)

  val allCharacters = Seq(Infantry, Tank)
  val infantryCharacters: Set[Character] = Set(Infantry)
}
