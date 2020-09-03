package com.wix.testPuzzle.model

import com.wix.testPuzzle.Direction

class FieldContainer(size: Int = 4) {

  private var field: Field = Field(size)

  def getCells: Seq[Seq[Int]] = field.getCells

  def reset(): Unit = {
    field = Field()
  }

  def moveEmptyCell(direction: Direction.Value):Boolean = {
    field.moveEmptyCell(direction)
  }

  def isSolved: Boolean = field.isSolved

}
