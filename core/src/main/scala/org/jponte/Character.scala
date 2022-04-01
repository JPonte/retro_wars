package org.jponte

case class Character(
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
  val Artillery: Character = Character("Artillery", 'A', 25, 3, 5, MovementType.Tread)
  val Rockets: Character = Character("Rockets", 'R', 50, 5, 5, MovementType.Tires)

  val allCharacters = Seq(Infantry, Tank, Artillery, Rockets)
  val infantryCharacters: Set[Character] = Set(Infantry)
}
