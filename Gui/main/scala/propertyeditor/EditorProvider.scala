package org.workcraft.gui.propertyeditor

import org.workcraft.util.Action
import scalaz.effect.IO


trait EditorProvider {
  def getEditor(close:IO[Unit]):SimpleCellEditor
}
