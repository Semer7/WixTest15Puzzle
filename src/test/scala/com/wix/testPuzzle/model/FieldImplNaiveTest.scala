package com.wix.testPuzzle.model

import org.scalatest.WordSpecLike

class FieldImplNaiveTest extends WordSpecLike {

  "Is increment last zero" when {
    val cellsRandomizer: CellsRandomizer = CellsRandomizerImpl
    "empty" should {
      "acc default true return true" in {
        val testObject = new FieldImplNaive(cellsRandomizer = cellsRandomizer)

        assert(testObject.isIncrementalLastZero(List.empty))
      }
    }

    "single element" should {
      "is 0" in {
        val testObject = new FieldImplNaive(cellsRandomizer = cellsRandomizer)
        val testSeq = List(0)

        assert(testObject.isIncrementalLastZero(testSeq))
      }

      "not 0" in {
        val testObject = new FieldImplNaive(cellsRandomizer = cellsRandomizer)
        val testSeq = List(1)

        assert(!testObject.isIncrementalLastZero(testSeq))
      }
    }

    "multiple elements" should {
      "incremental with last 0" in {
        val testObject = new FieldImplNaive(cellsRandomizer = cellsRandomizer)
        val testSeq = List(1,2,3,4,5,6,0)

        assert(testObject.isIncrementalLastZero(testSeq))
      }

      "not incremental with last 0" in {
        val testObject = new FieldImplNaive(cellsRandomizer = cellsRandomizer)
        val testSeq = List(1,2,3,5,4,6,0)

        assert(!testObject.isIncrementalLastZero(testSeq))
      }

      "incremental with last not 0" in {
        val testObject = new FieldImplNaive(cellsRandomizer = cellsRandomizer)
        val testSeq = List(1,2,3,5,4,6,7)

        assert(!testObject.isIncrementalLastZero(testSeq))
      }

      "not incremental with last not 0" in {
        val testObject = new FieldImplNaive(cellsRandomizer = cellsRandomizer)
        val testSeq = List(1,2,3,5,4,6,7)

        assert(!testObject.isIncrementalLastZero(testSeq))
      }
    }
  }

  "Fill cells" when {
    "filled with ordered data contain should same elements in same order" in {
      val topLimit = 16
      val seq = Range(0, topLimit)
      val cellsRandomizer: CellsRandomizer = new CellsRandomizer {
        override protected def getRandomUniqueInts(topLimit: Int, existing: Seq[Int]): Seq[Int] = { seq }

        override protected def isSolvable(size: Int, elementsQuantity: Int,  elems: Seq[Int]): Boolean = true
      }
      val testObject = new FieldImplNaive(cellsRandomizer = cellsRandomizer)

      assert(testObject.cells.flatten.toSeq == seq)
    }

    "filled with random data should no number is missed" in {
      val cellsRandomizer = CellsRandomizerImpl
      val testObject = new FieldImplNaive(cellsRandomizer = cellsRandomizer)

      assert(testObject.cells.flatten.sorted.toSeq == Range(0, 16))
    }
  }

  "Swap cells" when {
    val cellsRandomizer = CellsRandomizerImpl
    val testObject = new FieldImplNaive(4, cellsRandomizer = cellsRandomizer)

    "top left cell" should  {
      val sourceCell = (0, 0)

      "up not allowed" in {
        val direction = Direction.UP

        assert(!testObject.moveCell(sourceCell._1, sourceCell._2, direction))
      }

      "right" in {
        val direction = Direction.RIGHT

        val getSourceCellValue = () => testObject.cells(sourceCell._1)(sourceCell._2)
        val getTargeCellValue = () => testObject.cells(0)(1)

        val movedCellValue = getSourceCellValue()
        val targetCellValue = getTargeCellValue()

        assert(testObject.moveCell(sourceCell._1, sourceCell._2, direction))

        assert(getSourceCellValue() == targetCellValue)
        assert(getTargeCellValue() == movedCellValue)
      }

      "down" in {
        val direction = Direction.DOWN

        val getSourceCellValue = () => testObject.cells(sourceCell._1)(sourceCell._2)
        val getTargeCellValue = () => testObject.cells(1)(0)

        val movedCellValue = getSourceCellValue()
        val targetCellValue = getTargeCellValue()

        assert(testObject.moveCell(sourceCell._1, sourceCell._2, direction))

        assert(getSourceCellValue() == targetCellValue)
        assert(getTargeCellValue() == movedCellValue)
      }

      "left not allowed" in {
        val direction = Direction.UP

        assert(!testObject.moveCell(sourceCell._1, sourceCell._2, direction))
      }


    }

    "bottom right cell" should {
      val sourceCell = (3, 3)

      "up" in {
        val direction = Direction.UP

        val getSourceCellValue = () => testObject.cells(sourceCell._1)(sourceCell._2)
        val getTargeCellValue = () => testObject.cells(2)(3)

        val movedCellValue = getSourceCellValue()
        val targetCellValue = getTargeCellValue()

        assert(testObject.moveCell(sourceCell._1, sourceCell._2, direction))

        assert(getSourceCellValue() == targetCellValue)
        assert(getTargeCellValue() == movedCellValue)
      }

      "right not allowed" in {
        val direction = Direction.RIGHT

        assert(!testObject.moveCell(sourceCell._1, sourceCell._2, direction))
      }

      "down not allowed" in {
        val direction = Direction.DOWN

        assert(!testObject.moveCell(sourceCell._1, sourceCell._2, direction))
      }

      "left" in {
        val direction = Direction.LEFT

        val getSourceCellValue = () => testObject.cells(sourceCell._1)(sourceCell._2)
        val getTargeCellValue = () => testObject.cells(3)(2)

        val movedCellValue = getSourceCellValue()
        val targetCellValue = getTargeCellValue()

        assert(testObject.moveCell(sourceCell._1, sourceCell._2, direction))

        assert(getSourceCellValue() == targetCellValue)
        assert(getTargeCellValue() == movedCellValue)
      }
    }
  }
}
