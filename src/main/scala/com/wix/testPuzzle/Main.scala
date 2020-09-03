package com.wix.testPuzzle

import com.wix.testPuzzle.view.MainView

import scala.swing.{Frame, SimpleSwingApplication}

object Main extends SimpleSwingApplication {
  override def top: Frame = new MainView()
}