package org.jponte

case class Character(
    name: String,
    baseAttack: Int,
    minAttackRange: Int,
    maxAttackRange: Int,
    moveRange: Int,
    hasActionAfterMove: Boolean,
    movementType: MovementType,
    cost: Long
)

object Character {
  val Infantry: Character =
    Character("Infantry", 10, 1, 1, 3, true, MovementType.Foot, 1000)
  val Tank: Character = Character("Cavalry", 50, 1, 1, 5, true, MovementType.Tread, 5000)
  val Artillery: Character = Character("Archer", 25, 2, 3, 5, false, MovementType.Tread, 6000)
  val Rockets: Character = Character("Catapult", 50, 3, 5, 5, false, MovementType.Tires, 10000)

  val allCharacters = Seq(Infantry, Tank, Artillery, Rockets)
  val infantryCharacters: Set[Character] = Set(Infantry)
}
