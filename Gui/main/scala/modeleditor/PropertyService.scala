package org.workcraft.gui.modeleditor
import org.workcraft.services.SingleService
import org.workcraft.services.EditorScope
import org.workcraft.scala.Expressions.Expression
import org.workcraft.gui.propertyeditor.EditableProperty

object PropertyService extends SingleService[EditorScope, Expression[List[Expression[EditableProperty]]]]
