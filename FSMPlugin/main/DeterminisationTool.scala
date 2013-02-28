package org.workcraft.plugins.fsm

import org.workcraft.gui.MainWindow
import org.workcraft.gui.services.GuiTool
import org.workcraft.gui.services.ToolClass
import org.workcraft.scala.effects.IO._


class DeterminisationTool extends GuiTool {
  val classification = ToolClass.Conversion
  def run(mainWindow: MainWindow) = mainWindow.editorInFocus.expr.map(_.flatMap(_.content.model.implementation(NfaService)) match {
    case Some => 
    case None => ioPure.pure{}
  }
  )
}
