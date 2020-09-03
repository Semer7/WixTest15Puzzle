package com.wix.testPuzzle.view

import java.awt.Color

import scala.swing.{Dimension, MainFrame}

class MainView extends MainFrame {
  title = "Checkers puzzle"
  contents = new PlayView(4)
  preferredSize = new Dimension(640, 640)
  size = new Dimension(640, 640)

  background = Color.white

  resizable = false
  centerOnScreen()
}