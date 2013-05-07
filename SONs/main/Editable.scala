package org.workcraft.sons

import scalaz.{Node => _, _}
import Scalaz._

import org.workcraft.scala._
import org.workcraft.scala.Expressions._
import org.workcraft.graphics.Java2DDecoration._
import scalaz.effect.IO._
import scalaz.effect.IO
import org.workcraft.scala.effects._
import org.workcraft.services._
import org.workcraft.gui.modeleditor.EditorService

import core._

import OccNetEditor._

case class EditableOccurrenceNet[S,E](
  state : ModifiableExpression[(OccurrenceNet[S,E], Node[S,E] => Point)])

object EditableOccurrenceNet {
  def create[S,E](on : OccurrenceNet[S,E]) : IO[ModelServiceProvider] =
    for(
      versions <- newVar[VersionedState[S,E]](
        VersionStack.initial(EditorState((on, (x : Node[S,E]) => point(0,0)), Set())))
      ; tmp <- newVar[Option[EditorState[S,E]]](None)
      )
    yield new ModelServiceProvider {
      def implementation[T](service: Service[ModelScope, T]) = service match {
        case EditorService => Some(new OccNetEditor(versions, tmp))
        //    case LayoutableService => Some(PetriNetLayoutable(net))
        //    case DefaultFormatService => Some(Format.WorkcraftPetriNet)
        case s => s.monoid.zero
      }
    }
}
