package com.wix.testPuzzle.view

import java.awt.GridLayout

import com.wix.testPuzzle.model.FieldContainer
import javax.swing.BorderFactory

import scala.swing.event.{Key, KeyPressed}
import scala.swing.{Dialog, GridPanel}

class PlayView(size: Int) extends GridPanel(size, size) {

  val layout: GridLayout = peer.getLayout.asInstanceOf[GridLayout]
  layout.setHgap(10)
  layout.setVgap(10)

  border = BorderFactory.createEmptyBorder(20, 20, 20, 20)

  private val fieldContainer: FieldContainer = new FieldContainer()
  refresh()

  listenTo(keys)
  focusable = true
  requestFocus()

  private def refresh(): Unit = {
    contents.clear()

    contents ++= fieldContainer.getCells.flatMap(_.map(new CellView(_)))

    super.revalidate()
  }

  reactions += {
    case KeyPressed(_, key, _, _) =>
      import com.wix.testPuzzle.Direction._

      val direction = key match {
        case Key.Up => UP
        case Key.Right => RIGHT
        case Key.Down => DOWN
        case Key.Left => LEFT
      }

      if (fieldContainer.moveEmptyCell(direction)) {
        if (fieldContainer.isSolved) {
          val result:Dialog.Result.Value = Dialog.showConfirmation(this, "You WON! \n Start new game?", "HOORAY!", Dialog.Options.YesNo)

          if (result == Dialog.Result.Yes) {
            println("Starting new game")
            fieldContainer.reset()
            refresh()
          } else if (result == Dialog.Result.No || result == Dialog.Result.Closed) {
            println("Dialog is closed")
          }
        }
      } else {

      }

      refresh()
  }
}
