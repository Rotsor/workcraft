package org.workcraft.gui.modeleditor
import org.workcraft.services.SingleService
import org.workcraft.services.EditorScope
import org.workcraft.gui.modeleditor.tools.ModelEditorTool
import org.workcraft.gui.modeleditor.tools.ModelEditorToolInstance
import scalaz.effect.IO
import org.workcraft.gui.modeleditor.tools.ToolEnvironment

object ShowTraceService extends SingleService[EditorScope, ShowTrace]

trait ShowTrace {
  def show (trace: List[String]): (ModelEditorTool, ToolEnvironment => IO[ModelEditorToolInstance])
}
