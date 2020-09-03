package com.wix.testPuzzle.model

import com.wix.testPuzzle.Direction
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

  "Move empty cell" when {

    "top left cell" should  {
      val cellsRandomizer = new CellsRandomizer {
        override protected def getRandomUniqueInts(elementsQuantity: Int, existing: Seq[Int]): Seq[Int] = 0 until 16

        override protected def isSolvable(size: Int, elementsQuantity: Int, elems: Seq[Int]): Boolean = true
      }
      val testObject = Field(4, cellsRandomizer).asInstanceOf[FieldImplNaive]

      "up not allowed" in {
        val testObject = Field(4, cellsRandomizer).asInstanceOf[FieldImplNaive]
        val direction = Direction.UP

        assert(!testObject.moveEmptyCell(direction))
      }

      "right" in {
        val testObject = Field(4, cellsRandomizer).asInstanceOf[FieldImplNaive]
        val direction = Direction.RIGHT

        assert(testObject.moveEmptyCell(direction))
        assert(findZeroCell(testObject.cells) == Coordinates(0, 1))
      }

      "down" in {
        val testObject = Field(4, cellsRandomizer).asInstanceOf[FieldImplNaive]
        val direction = Direction.DOWN

        assert(testObject.moveEmptyCell(direction))
        assert(findZeroCell(testObject.cells) == Coordinates(1, 0))
      }

      "left not allowed" in {
        val testObject = Field(4, cellsRandomizer).asInstanceOf[FieldImplNaive]
        val direction = Direction.LEFT

        assert(!testObject.moveEmptyCell(direction))
      }


    }

    "bottom right cell" should {
      val cellsRandomizer = new CellsRandomizer {
        override protected def getRandomUniqueInts(elementsQuantity: Int, existing: Seq[Int]): Seq[Int] = 15 to 0 by -1

        override protected def isSolvable(size: Int, elementsQuantity: Int, elems: Seq[Int]): Boolean = true
      }

      "up" in {
        val testObject = Field(4, cellsRandomizer).asInstanceOf[FieldImplNaive]
        val direction = Direction.UP

        assert(testObject.moveEmptyCell(direction))
        assert(findZeroCell(testObject.cells) == Coordinates(2, 3))
      }

      "right not allowed" in {
        val testObject = Field(4, cellsRandomizer).asInstanceOf[FieldImplNaive]
        val direction = Direction.RIGHT

        assert(!testObject.moveEmptyCell(direction))
      }

      "down not allowed" in {
        val testObject = Field(4, cellsRandomizer).asInstanceOf[FieldImplNaive]
        val direction = Direction.DOWN

        assert(!testObject.moveEmptyCell(direction))
      }

      "left" in {
        val testObject = Field(4, cellsRandomizer).asInstanceOf[FieldImplNaive]
        val direction = Direction.LEFT

        assert(testObject.moveEmptyCell(direction))
        assert(findZeroCell(testObject.cells) == Coordinates(3, 2))
      }
    }
  }

  def findZeroCell(array: Array[Array[Int]]): Coordinates = {
    val (columnWithZero, row) = array.zipWithIndex.filter(tuple => tuple._1.contains(0)).head

    Coordinates(row, columnWithZero.zipWithIndex.filter(_._1 == 0).head._2)
  }
}
