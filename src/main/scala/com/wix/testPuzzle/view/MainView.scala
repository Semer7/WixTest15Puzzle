package com.wix.testPuzzle.view

import java.awt.{Color, GridLayout}

import com.wix.testPuzzle.model.{Direction, FieldContainer}
import javax.swing.BorderFactory

import scala.swing.event.{Key, KeyPressed}
import scala.swing.{Dialog, Dimension, Font, GridPanel, Label, MainFrame}

class MainView extends MainFrame {
  title = "Checkers puzzle"
  contents = new PlayView(4)
  preferredSize = new Dimension(640, 640)
  size = new Dimension(640, 640)

  background = Color.white

  resizable = false
  centerOnScreen()
}

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
      import Direction._

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

class CellView(value:Int) extends Label(if (value == 0) "" else value.toString) {

  opaque = true
  background = Color.WHITE

  border = BorderFactory.createEtchedBorder()
  peer.setMinimumSize(new Dimension(50, 50))
  peer.setPreferredSize(new Dimension(90, 90))
  peer.setMaximumSize(new Dimension (120, 120))

  foreground = Color.BLUE
  font = Font.apply(Font.SansSerif, Font.Style.Bold, 40)
}
