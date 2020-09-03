package com.wix.testPuzzle.model

import org.scalatest.WordSpecLike

class CellsRandomizerTest extends WordSpecLike {

  "Cells randomizer" when {
    "get random unique integers" should {
      "provide random unique integers 0 to limit" in {
        val limit = 16
        val cellsRandomizer: CellsRandomizer = CellsRandomizerImpl

        val randomNumbers = cellsRandomizer.getValidRandomInts(limit)
        val distinctNumbers = randomNumbers.distinct

        assert(randomNumbers == distinctNumbers)
      }
    }

    "Check if numbers provided are solvable" should {
      "return true if solvable" in {
        assert(CellsRandomizerImpl.isSolvable(4, 16, Seq(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,0)))
        assert(CellsRandomizerImpl.isSolvable(4, 16, Seq(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,0)))
        assert(CellsRandomizerImpl.isSolvable(4, 16, Seq(7,8,9,10,6,1,2,11,5,4,3,12,0,15,14,13)))
      }

      "return false if unsolvable" in {
        assert(!CellsRandomizerImpl.isSolvable(4, 16, Seq(1,2,3,4,5,6,7,8,9,10,11,12,13,15,14,0)))
        assert(!CellsRandomizerImpl.isSolvable(4, 16, Seq(15,14,13,12,11,10,9,8,7,6,5,4,3,2,1,0)))
        assert(!CellsRandomizerImpl.isSolvable(4, 16, Seq(4,3,2,1,5,14,13,12,6,15,0,11,7,8,9,10)))

      }
    }
  }

}
