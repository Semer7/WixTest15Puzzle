package com.wix.testPuzzle.model

import scala.util.Random

trait CellsRandomizer {

  def getValidRandomInts(size: Int = 4): Seq[Int] = {
    val elementsQuantity = size * size
    val result = getRandomUniqueInts(elementsQuantity, existing = Seq.empty)

    if (isSolvable(size, elementsQuantity, result)) result
    else getValidRandomInts(size)
  }

  protected def getRandomUniqueInts(elementsQuantity: Int, existing: Seq[Int]): Seq[Int]

  protected def isSolvable(size: Int, elementsQuantity: Int, elems: Seq[Int]):Boolean
}

object CellsRandomizerImpl extends CellsRandomizer {

  @scala.annotation.tailrec
  override def getRandomUniqueInts(elementsQuantity: Int, existing: Seq[Int]): Seq[Int] = {
    if (existing.size == elementsQuantity) existing
    else {
      val randInt = Random.nextInt(elementsQuantity)

      val newExisting = if (existing.contains(randInt)) existing
      else existing.appended(randInt)

      getRandomUniqueInts(elementsQuantity, newExisting)
    }
  }

  override def isSolvable(size: Int, elementsQuantity: Int, elems: Seq[Int]):Boolean = {
    countChaoticNumber(size, elementsQuantity, seq = elems) % 2 == 0
  }

  @scala.annotation.tailrec
  def countChaoticNumber(size: Int, elementsQuantity: Int, seq: Seq[Int], chaoticNumber: Int = 0): Int = {
    if (seq.isEmpty) chaoticNumber
    else {
      val head = seq.head
      val tail = seq.tail

      val chaoticIncrement =
        if (head == 0) {
          val currentElemIndex = ((elementsQuantity - seq.length) / size) + 1
          currentElemIndex
        } else {
          tail.foldLeft(0)((counter, elem) => {
            if (elem != 0 && elem < head) counter + 1
            else counter
          })
        }

      countChaoticNumber(size, elementsQuantity, tail, chaoticNumber + chaoticIncrement)
    }
  }
}
