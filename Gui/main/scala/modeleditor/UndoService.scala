package org.workcraft.gui.modeleditor
import org.workcraft.services._
import org.workcraft.scala.Expressions.Expression
import scalaz.effect.IO

object UndoService extends SingleService[EditorScope, Undo]

case class Undo (undo: Expression[Option[UndoAction]], redo: Expression[Option[UndoAction]]) 

case class UndoAction (description: String, action: IO[Unit])
