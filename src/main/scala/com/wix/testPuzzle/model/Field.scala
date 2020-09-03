package com.wix.testPuzzle.model

import com.wix.testPuzzle.Direction

trait Field {

  def isSolved: Boolean

  def getCells: Seq[Seq[Int]]

  def moveEmptyCell(direction: Direction.Value): Boolean
}

object Field {

  def apply(size: Int = 4, cellsRandomizer: CellsRandomizer = CellsRandomizerImpl): Field =
    new FieldImplNaive(size, cellsRandomizer)
}

private class FieldImplNaive(size: Int = 4, cellsRandomizer: CellsRandomizer) extends Field {

  private[model] val cells: Array[Array[Int]] = fillCells
  private var cZero: Coordinates = {
    (for ((rowCells, row) <- cells.zipWithIndex;
         (cell, col) <- rowCells.zipWithIndex
         if cell == 0)
      yield Coordinates(row, col)).head
  }

  def getCells: Seq[Seq[Int]] = cells.toSeq.map(_.toSeq)

  override def isSolved: Boolean =
    cells(0)(0) == 1 && isIncrementalLastZero(cells.flatten.toList) // checking for 1 to simplify calculations a bit

  def fillCells: Array[Array[Int]] =
    cellsRandomizer.getValidRandomInts(size)
      .foldLeft((Array.ofDim[Int](size, size), 0))((resultTuple, elem) => {
        val counter = resultTuple._2
        val row = counter / size
        val column = counter % size

        val result = resultTuple._1

        result(row)(column) = elem
        (result, counter + 1)
      })._1

  @scala.annotation.tailrec
  final def isIncrementalLastZero(seq: List[Int]): Boolean = {
    seq match {
      case Nil => true
      case 0 :: Nil => true
      case x :: y :: 0 :: Nil =>
        y - x == 1
      case x :: y :: _ =>
        if (y - x == 1) isIncrementalLastZero(seq.tail)
        else false
      case _ =>
        false
    }
  }

  def moveEmptyCell(direction: Direction.Value): Boolean = {
    import com.wix.testPuzzle.Direction._
    direction match {
      case UP if cZero.row > 0 =>
        cZero = switchCells(cZero, cZero.prevRow)
        true
      case RIGHT if cZero.column < size - 1 =>
        cZero = switchCells(cZero, cZero.nextColumn)
        true
      case DOWN if cZero.row < size - 1 =>
        cZero = switchCells(cZero, cZero.nextRow)
        true
      case LEFT if cZero.column > 0 =>
        cZero = switchCells(cZero, cZero.prevColumn)
        true
      case _ => false
    }
  }

  /**
    Changes values between cells

   @param from source cell coordinates
   @param to destination cell coordinates

   @return destination coordinates


   */
  def switchCells(from: Coordinates, to: Coordinates): Coordinates = {
    val tempValue = getCellBy(from)
    updateCellBy(from, getCellBy(to))
    updateCellBy(to, tempValue)
    to
  }

  private def getCellBy(coordinates: Coordinates) =
    cells(coordinates.row)(coordinates.column)
  private def updateCellBy(coordinates: Coordinates, value: Int): Unit =
    cells(coordinates.row).update(coordinates.column, value)
}

case class Coordinates(row: Int, column: Int) {

  def prevRow: Coordinates = copy(row = row - 1)
  def nextRow: Coordinates = copy(row = row + 1)

  def prevColumn: Coordinates = copy(column = column - 1)
  def nextColumn: Coordinates = copy(column = column + 1)
}

