package org.workcraft.gui.services
import scalaz.effect.IO
import org.workcraft.scala.Expressions._
import org.workcraft.services.MultiService
import org.workcraft.services.ModelServiceProvider
import org.workcraft.gui.MainWindow
import org.workcraft.services.GlobalScope

object GuiToolService extends MultiService[GlobalScope, GuiTool]

sealed abstract class ToolClass(val sectionName: String)

object ToolClass {
  object Verification extends ToolClass ("Verification")
  object Layout extends ToolClass ("Layout")
  object Synthesis extends ToolClass ("Synthesis")
  object Conversion extends ToolClass ("Conversion")
  case class Custom (secName: String) extends ToolClass(secName)
}

trait GuiTool {
  val description: String
  val classification: ToolClass
  def run (mainWindow: MainWindow): Expression[Option[IO[Unit]]]
}
