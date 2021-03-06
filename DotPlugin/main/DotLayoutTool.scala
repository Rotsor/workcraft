package org.workcraft.plugins.dot
import org.workcraft.gui.services.GuiTool
import org.workcraft.gui.services.ToolClass
import org.workcraft.gui.MainWindow
import org.workcraft.scala.Expressions
import scalaz.effect.IO
import scalaz.effect.IO._
import org.workcraft.scala.Expressions._
import scalaz.Scalaz._
import java.io.File
import org.workcraft.services.ExportError
import org.workcraft.plugins.dot.parser.Dot
import org.workcraft.plugins.dot.parser.Node
import org.workcraft.gui.tasks.ModalTaskDialog
import org.workcraft.tasks.Task._
import javax.swing.JOptionPane
import org.workcraft.services.layout.LayoutableService
import org.workcraft.services.layout.Layoutable
import org.workcraft.services.layout.Layouter
import org.workcraft.services.layout.LayoutSpec
import org.workcraft.plugins.dot.parser.Attr
import java.awt.geom.Point2D
import java.awt.geom.AffineTransform
import org.workcraft.tasks.Task
import org.workcraft.tasks.File._

sealed trait DotError

object DotError {
  case class CouldNotStart(cause: Throwable) extends DotError
  case class RuntimeError(text: String) extends DotError
  case class DotExportError(cause: ExportError) extends DotError
  case class DotParseError(cause: String) extends DotError
}

object DotLayoutTool extends GuiTool {
  import DotError._

  val description = "Layout using dot"
  val classification = ToolClass.Layout
  
  def parsePos (pos: Option[String], transform: AffineTransform) = {
    val Array(x, y) = pos.getOrElse("0,0").split(",").map(_.toDouble)
    val res = new Point2D.Double (x / 72.0, y / 72.0)
    transform.transform(res, res)
    res
  }

  def layouter[E >: DotError] = new Layouter[({type T[A]=Task[A, E]})#T] {
      def layout[N](spec : LayoutSpec[N]) = {
        withTempFile("workcraft",".dot",input => 
          withTempFile("workcraft",".dot",output => {
            val export = new DotExportJob(spec).asTask(input).mapError(DotExportError(_))
            val parse = Dot.parseTask(output).mapError(DotParseError(_))
            val layoutTask = new DotLayoutTask("./tools/dot/dot", input, output)

            export *>
              layoutTask *>
              parse map {
                graph => {
                  val nodeToId = spec.nodes.zipWithIndex.toMap
                  val idToNode = nodeToId.map(_.swap).toMap

                  graph.statements.map {
                    case Node(id, _, attrs @ _ *) =>
                      Some((idToNode(id.toInt),
                        parsePos(attrs.find(_.name == "pos").map(_.value.get),
                          spec.orientation.transform)))
                    case _ => None
                  }.flatten.toList
                }
              }
          }
          )
        )
      }
  }

  def task(layoutable : Layoutable) : Task [Unit, DotError] = {
    layoutable.apply[({type T[A]=Task[A, DotError]})#T](layouter)
  }

  def run(mainWindow: MainWindow) =
    mainWindow.editorInFocus.expr.map(
      editorWindow => editorWindow.flatMap(_.content.model.implementation(LayoutableService))
        .map(
          (l => {
            ModalTaskDialog.runTask(mainWindow, "Generating layout using dot", task(l)) >>=
             {
              case Left(None) => 
                IO { JOptionPane.showMessageDialog(mainWindow, "Cancelled") }
              case Left(Some(error)) => 
                IO { JOptionPane.showMessageDialog(mainWindow, error, "Error", JOptionPane.ERROR_MESSAGE) }
              case Right(()) => IO { }
            }})
      )
    )
}
