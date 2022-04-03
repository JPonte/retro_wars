package org.jponte

import indigoextras.ui.Button

sealed trait UIState

case class OverviewState(
    hoverTile: Option[Position],
    endTurnButton: Button,
    checkRange: Option[Position]
) extends UIState

case class UnitMoveState(
    hoverTile: Option[Position],
    selectedPosition: Position,
    movableTiles: Set[Position],
    movingPath: Seq[Position]
) extends UIState

case class UnitActionState(
    selectedPosition: Position,
    movingPath: Seq[Position],
    actionButtons: ActionBox,
    targets: Set[Position]
) extends UIState

case class UnitAttackState(
    selectedPosition: Position,
    hoverTile: Option[Position],
    movingPath: Seq[Position],
    targets: Set[Position]
) extends UIState

case class PurchaseUnitActionState(
    selectedPosition: Position,
    purchaseMenu: PurchaseMenu
) extends UIState
