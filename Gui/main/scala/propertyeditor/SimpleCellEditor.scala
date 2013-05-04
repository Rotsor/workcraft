package org.workcraft.gui.propertyeditor

import java.awt.Component
import scalaz.effect.IO

trait SimpleCellEditor {
  def commit: IO[Option[String]]
  def getComponent(): Component
}