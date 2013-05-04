package org.workcraft.gui.propertyeditor

import org.workcraft.util.Action

import scalaz.effect.IO


trait GenericEditorProvider[T] {
  def createEditor(initialValue:T, accept: IO[Unit], cancel: IO[Unit]): GenericCellEditor[T]
}