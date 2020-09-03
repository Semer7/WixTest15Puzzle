package com.wix.testPuzzle.view

import java.awt.Color

import javax.swing.BorderFactory

import scala.swing.{Dimension, Font, Label}

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