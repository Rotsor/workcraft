package org.workcraft.gui.modeleditor
import org.workcraft.services.Service
import org.workcraft.services.ModelScope
import scalaz.NonEmptyList
import tools._
import tools.ModelEditorTool.ModelEditorToolConstructor
import org.workcraft.scala.Expressions.Expression
import org.workcraft.gui.propertyeditor.EditableProperty

object EditorService extends Service[ModelScope, ModelEditor]

trait ModelEditor {
  def tools: NonEmptyList[ModelEditorToolConstructor]
  def props : Expression[List[EditableProperty]]
}
