package org.workcraft.sons

import core._
import org.workcraft.gui.modeleditor.tools.ConnectionManager
import org.workcraft.gui.modeleditor.tools.Button
import org.workcraft.gui.modeleditor.tools.NodeGeneratorTool
import org.workcraft.gui.modeleditor.KeyBinding
import org.workcraft.gui.modeleditor.KeyEventType
import org.workcraft.gui.modeleditor.ModelEditor
import org.workcraft.scala.grapheditor.tools.GenericSelectionTool
import org.workcraft.scala.effects._
import org.workcraft.scala._
import org.workcraft.scala.Expressions._
import org.workcraft.services.Service
import org.workcraft.services.EditorScope

import scalaz.{Node=>_, Scalaz=>_, _}
import scalaz.Scalaz._
import scalaz.effect.IO
import scalaz.effect.IO._

object OccNetEditor {
  case class EditorState[S,E] (net : VisualOccurrenceNet[S,E], selection : Set[Node[S,E]])
  def updsel[S,E](state : EditorState[S,E], sel : Set[Node[S,E]]) : EditorState[S,E] = {
    state.copy(selection = sel)
  }
  def ttt[S,E] : Unit = {
    val a : EditorState[S,E] = null
    val q : Set[Node[S,E]] = null
    val b = a.copy(selection = q)
  }
  type VersionedState[S,E] = VersionStack[EditorState[S,E]]
}
import OccNetEditor._

class OccNetEditor[S,E](
  versions: ModifiableExpression[VersionedState[S,E]],
  tmp : ModifiableExpression[Option[EditorState[S,E]]]
  ) extends ModelEditor {

  val currentVersion = ModifiableExpression(
    tmp >>= {
      case None => versions.map(_.current._1)
      case Some(x) => x.pure[Expression]
    },
    (x : EditorState[S,E]) => tmp.set(Some(x))
  )

  val nodes = currentVersion.map(_.net._1.nodes)

  val selection : ModifiableExpression[Set[Node[S,E]]] = currentVersion.refract(_.selection, {
    (sel : Set[Node[S,E]]) => (s : EditorState[S,E]) =>
      s.copy(selection = sel)
  }
    )
  def transaction(msg : String, f : EditorState[S,E] => EditorState[S,E]) : IO[Unit] = {
    versions.update(s => s.record(f(s.current._1), msg))
  }
  def commit(msg : String) : IO[Unit] = {
    for(
      tmpState <- currentVersion.eval;
      versionsV <- versions.update(_.record(tmpState, msg));
      _ <- tmp.set(None)
    ) yield ()
  }

  def selectionTool = GenericSelectionTool[Node[S,E]](
    nodes,
    selection,
    null,
    (_, x) => x,
    null,
    null,
    null /*List(
      KeyBinding("Delete selection", KeyEvent.VK_DELETE, KeyEventType.KeyPressed, Set(), pushUndo("delete nodes") >> selection.eval >>= (sel => selection.update(_ -- sel) >> net.deleteNodes(sel) >| None)),
      KeyBinding("Toggle marking", KeyEvent.VK_SPACE, KeyEventType.KeyPressed, Set(), pushUndo("toggle marking") >> (selection.eval >>= (sel => toggleSelectionMarking(sel))) >| None)
    )*/,
    Some (null))

  def tools = null // NonEmptyList(selectionTool, connectionTool, stateGeneratorTool, eventGeneratorTool, simulationTool)

  def implementation[T](service: Service[EditorScope, T]) =
    service.monoid.zero
}
